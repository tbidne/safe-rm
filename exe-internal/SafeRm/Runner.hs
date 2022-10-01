-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    runSafeRmHandler,

    -- * Helpers
    FinalConfig (..),
    getConfiguration,
  )
where

import Data.ByteString qualified as BS
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import SafeRm qualified
import SafeRm.Args
  ( Args (command, tomlConfigPath),
    SafeRmCommand
      ( SafeRmCommandDelete,
        SafeRmCommandEmpty,
        SafeRmCommandList,
        SafeRmCommandMetadata,
        SafeRmCommandPermDelete,
        SafeRmCommandRestore
      ),
    getArgs,
  )
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
  )
import SafeRm.Prelude
import SafeRm.Toml (TomlConfig (trashHome), mergeConfigs)
import System.Directory (XdgDirectory (XdgConfig))
import System.Directory qualified as Dir
import TOML qualified

-- | Reads cli args and prints the results to stdout.
--
-- @since 0.1
runSafeRm :: IO ()
runSafeRm = runSafeRmHandler (putStrLn . T.unpack)

-- | Reads CLI args and applies the parameter handler.
--
-- @since 0.1
runSafeRmHandler :: (Text -> IO ()) -> IO ()
runSafeRmHandler handler = do
  -- Combine args and toml config to get final versions of shared config
  -- values. Right now, only the trash home is shared.
  finalConfig <- getConfiguration
  let finalTrashHome = finalConfig ^. #trashHome

  case finalConfig ^. #command of
    SafeRmCommandDelete paths -> SafeRm.delete finalTrashHome (listToSet paths)
    SafeRmCommandPermDelete force paths ->
      SafeRm.deletePermanently finalTrashHome force (listToSet paths)
    SafeRmCommandEmpty -> SafeRm.empty finalTrashHome
    SafeRmCommandRestore paths ->
      SafeRm.restore finalTrashHome (listToSet paths)
    SafeRmCommandList -> do
      listIndex handler finalTrashHome
      printStats handler finalTrashHome
    SafeRmCommandMetadata -> printStats handler finalTrashHome

-- | Holds the final configuration data.
--
-- @since 0.1
data FinalConfig = MkFinalConfig
  { trashHome :: !(Maybe (PathI TrashHome)),
    command :: !SafeRmCommand
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the full
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
--
-- @since 0.1
getConfiguration :: IO FinalConfig
getConfiguration = do
  -- get CLI args
  args <- getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    Just tomlPath -> readConfig tomlPath
    -- no toml config path given...
    Nothing -> do
      xdgConfig <- Dir.getXdgDirectory XdgConfig "safe-rm"
      let defPath = xdgConfig </> "config.toml"
      exists <- Dir.doesFileExist defPath
      if exists
        then -- 2. config exists at default path: read
          readConfig defPath
        else -- 3. no config exists: return default (empty)
          pure mempty

  -- merge share CLI and toml values
  let tomlConfig' = mergeConfigs args tomlConfig
  pure $
    MkFinalConfig
      { trashHome = tomlConfig' ^. #trashHome,
        command = args ^. #command
      }
  where
    readConfig fp = do
      contents <-
        BS.readFile fp >>= \contents' -> do
          case TEnc.decodeUtf8' contents' of
            Right txt -> pure txt
            Left err -> throwIO err
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkExceptionI @TomlDecode tomlErr

listIndex :: (Text -> IO a) -> Maybe (PathI TrashHome) -> IO a
listIndex = prettyDel SafeRm.getIndex

printStats :: (Text -> IO a) -> Maybe (PathI TrashHome) -> IO a
printStats = prettyDel SafeRm.getMetadata

prettyDel :: Pretty b => (a -> IO b) -> (Text -> IO c) -> a -> IO c
prettyDel f handler =
  handler
    . renderStrict
    . layoutCompact
    . pretty
    <=< f

listToSet :: Hashable a => NonEmpty a -> HashSet a
listToSet = Set.fromList . NE.toList
