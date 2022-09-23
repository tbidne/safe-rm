{-# LANGUAGE CPP #-}

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
  let action = case command of
        DelCommandDelete mtrash paths -> Del.del mtrash (setToMap paths)
        DelCommandPermDelete mtrash paths ->
          Del.permDel mtrash (setToMap paths)
        DelCommandEmpty mtrash -> Del.empty mtrash
        DelCommandRestore mtrash paths ->
          Del.restore mtrash (setToMap paths)
        DelCommandList mtrash -> do
          listIndex handler mtrash
          printStats handler mtrash
        DelCommandStats mtrash -> printStats handler mtrash
  action `catchSync` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx = handler . T.pack . displayException

listIndex :: (Text -> IO a) -> Maybe FilePath -> IO a
listIndex handler = prettyDel Del.getIndex handler

printStats :: (Text -> IO a) -> Maybe FilePath -> IO a
printStats handler = prettyDel Del.getStatistics handler

prettyDel :: Pretty b => (a -> IO b) -> (Text -> IO c) -> a -> IO c
prettyDel f handler =
  handler
    . renderStrict
    . layoutCompact
    . pretty
    <=< f

setToMap :: Hashable a => NonEmpty a -> HashSet a
setToMap = Set.fromList . NE.toList

catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex
