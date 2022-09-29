-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( runSafeRm,
    runSafeRmHandler,
  )
where

import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import SafeRm qualified
import SafeRm.Args
  ( Args (command),
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
import SafeRm.Prelude

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
  command <- view #command <$> getArgs
  case command of
    SafeRmCommandDelete mtrash paths -> SafeRm.delete mtrash (listToSet paths)
    SafeRmCommandPermDelete mtrash force paths ->
      SafeRm.deletePermanently mtrash force (listToSet paths)
    SafeRmCommandEmpty mtrash -> SafeRm.empty mtrash
    SafeRmCommandRestore mtrash paths ->
      SafeRm.restore mtrash (listToSet paths)
    SafeRmCommandList mtrash -> do
      listIndex handler mtrash
      printStats handler mtrash
    SafeRmCommandMetadata mtrash -> printStats handler mtrash

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
