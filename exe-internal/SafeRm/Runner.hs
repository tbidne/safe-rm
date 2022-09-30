-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( runSafeRm,
    runSafeRmHandler,
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

  -- Combine args and toml config to get final versions of shared config
  -- values. Right now, only the trash home is shared.
  let finalTrashHome = mergeConfigs args tomlConfig ^. #trashHome

  case args ^. #command of
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
