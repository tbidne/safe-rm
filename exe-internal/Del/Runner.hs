-- | This modules provides an executable for running del.
--
-- @since 0.1
module Del.Runner
  ( runDel,
    runDelHandler,
  )
where

import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Del qualified
import Del.Args
  ( Args (command),
    DelCommand
      ( DelCommandDelete,
        DelCommandEmpty,
        DelCommandList,
        DelCommandPermDelete,
        DelCommandRestore,
        DelCommandStats
      ),
    getArgs,
  )
import Del.Data.Paths (PathI, PathIndex (..))
import Del.Prelude

-- | Reads cli args and prints the results to stdout.
--
-- @since 0.1
runDel :: IO ()
runDel = runDelHandler (putStrLn . T.unpack)

-- | Reads CLI args and applies the parameter handler.
--
-- @since 0.1
runDelHandler :: (Text -> IO ()) -> IO ()
runDelHandler handler = do
  command <- view #command <$> getArgs
  case command of
    DelCommandDelete mtrash paths -> Del.del mtrash (listToSet paths)
    DelCommandPermDelete mtrash force paths ->
      Del.permDel mtrash force (listToSet paths)
    DelCommandEmpty mtrash -> Del.empty mtrash
    DelCommandRestore mtrash paths ->
      Del.restore mtrash (listToSet paths)
    DelCommandList mtrash -> do
      listIndex handler mtrash
      printStats handler mtrash
    DelCommandStats mtrash -> printStats handler mtrash

listIndex :: (Text -> IO a) -> Maybe (PathI TrashHome) -> IO a
listIndex = prettyDel Del.getIndex

printStats :: (Text -> IO a) -> Maybe (PathI TrashHome) -> IO a
printStats = prettyDel Del.getStatistics

prettyDel :: Pretty b => (a -> IO b) -> (Text -> IO c) -> a -> IO c
prettyDel f handler =
  handler
    . renderStrict
    . layoutCompact
    . pretty
    <=< f

listToSet :: Hashable a => NonEmpty a -> HashSet a
listToSet = Set.fromList . NE.toList
