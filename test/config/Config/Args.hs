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
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Env (Env (trashHome))
import SafeRm.Runner (getConfiguration)
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
  expectedPaths @=? cmd ^? _SafeRmCommandDelete
  where
    argList = ["d", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _SafeRmCommandPermDelete
  where
    argList = ["x", "foo", "bar"]
    expectedPaths = Just (False, "foo" :| ["bar"])

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _SafeRmCommandPermDelete
  where
    argList = ["x", "-f", "foo", "bar"]
    expectedPaths = Just (True, "foo" :| ["bar"])

empty :: TestTree
empty = testCase "Parses empty" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just False @=? cmd ^? _SafeRmCommandEmpty
  where
    argList = ["e"]

emptyForce :: TestTree
emptyForce = testCase "Parses empty with force" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just True @=? cmd ^? _SafeRmCommandEmpty
  where
    argList = ["e", "-f"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  expectedPaths @=? cmd ^? _SafeRmCommandRestore
  where
    argList = ["r", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

list :: TestTree
list = testCase "Parses list" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just () @=? cmd ^? _SafeRmCommandList
  where
    argList = ["l"]

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  defTrash <- getDefaultTrash
  (env, cmd) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  Just () @=? cmd ^? _SafeRmCommandMetadata
  where
    argList = ["m"]
