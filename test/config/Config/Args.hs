-- | Configuration Tests.
--
-- @since 0.1
module Config.Args
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Args
  ( _SafeRmCommandDelete,
    _SafeRmCommandEmpty,
    _SafeRmCommandList,
    _SafeRmCommandMetadata,
    _SafeRmCommandPermDelete,
    _SafeRmCommandRestore,
  )
import SafeRm.Runner (FinalConfig (command, trashHome), getConfiguration)
import System.Environment qualified as SysEnv

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Args"
    [ delete,
      permDelete,
      permDeleteForce,
      empty,
      emptyForce,
      restore,
      list,
      metadata
    ]

delete :: TestTree
delete = testCase "Parses delete" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  expectedPaths @=? finalConfig ^? (#command % _SafeRmCommandDelete)
  where
    argList = ["d", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  expectedPaths @=? finalConfig ^? (#command % _SafeRmCommandPermDelete)
  where
    argList = ["x", "foo", "bar"]
    expectedPaths = Just (False, "foo" :| ["bar"])

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  expectedPaths @=? finalConfig ^? (#command % _SafeRmCommandPermDelete)
  where
    argList = ["x", "-f", "foo", "bar"]
    expectedPaths = Just (True, "foo" :| ["bar"])

empty :: TestTree
empty = testCase "Parses empty" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  Just False @=? finalConfig ^? (#command % _SafeRmCommandEmpty)
  where
    argList = ["e"]

emptyForce :: TestTree
emptyForce = testCase "Parses empty with force" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  Just True @=? finalConfig ^? (#command % _SafeRmCommandEmpty)
  where
    argList = ["e", "-f"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  expectedPaths @=? finalConfig ^? (#command % _SafeRmCommandRestore)
  where
    argList = ["r", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

list :: TestTree
list = testCase "Parses list" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  Just () @=? finalConfig ^? (#command % _SafeRmCommandList)
  where
    argList = ["l"]

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  Just () @=? finalConfig ^? (#command % _SafeRmCommandMetadata)
  where
    argList = ["m"]
