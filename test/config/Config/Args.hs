-- | Configuration Tests.
--
-- @since 0.1
module Config.Args
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Env (Env (trashHome))
import SafeRm.Runner (getConfiguration)
import SafeRm.Runner.Command
  ( _Delete,
    _DeletePerm,
    _Empty,
    _List,
    _Metadata,
    _Restore,
  )
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
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _Delete
  where
    argList = ["d", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _DeletePerm
  where
    argList = ["x", "foo", "bar"]
    expectedPaths = Just (False, "foo" :| ["bar"])

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _DeletePerm
  where
    argList = ["x", "-f", "foo", "bar"]
    expectedPaths = Just (True, "foo" :| ["bar"])

empty :: TestTree
empty = testCase "Parses empty" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just False @=? cmd ^? _Empty
  where
    argList = ["e"]

emptyForce :: TestTree
emptyForce = testCase "Parses empty with force" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just True @=? cmd ^? _Empty
  where
    argList = ["e", "-f"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _Restore
  where
    argList = ["r", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

list :: TestTree
list = testCase "Parses list" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just () @=? cmd ^? _List
  where
    argList = ["l"]

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just () @=? cmd ^? _Metadata
  where
    argList = ["m"]
