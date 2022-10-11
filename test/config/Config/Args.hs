-- | Configuration Tests.
--
-- @since 0.1
module Config.Args
  ( tests,
  )
where

import Config.Prelude
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
      emptyTrash,
      emptyTrashForce,
      restore,
      list,
      metadata
    ]

delete :: TestTree
delete = testCase "Parses delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  expectedPaths @=? cmd ^? _Delete
  where
    argList = ["d", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

permDelete :: TestTree
permDelete = testCase "Parses perm delete" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  expectedPaths @=? cmd ^? _DeletePerm
  where
    argList = ["x", "foo", "bar"]
    expectedPaths = Just (False, "foo" :| ["bar"])

permDeleteForce :: TestTree
permDeleteForce = testCase "Parses perm delete with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  expectedPaths @=? cmd ^? _DeletePerm
  where
    argList = ["x", "-f", "foo", "bar"]
    expectedPaths = Just (True, "foo" :| ["bar"])

emptyTrash :: TestTree
emptyTrash = testCase "Parses empty" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just False @=? cmd ^? _Empty
  where
    argList = ["e"]

emptyTrashForce :: TestTree
emptyTrashForce = testCase "Parses empty with force" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just True @=? cmd ^? _Empty
  where
    argList = ["e", "-f"]

restore :: TestTree
restore = testCase "Parses restore" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  expectedPaths @=? cmd ^? _Restore
  where
    argList = ["r", "foo", "bar"]
    expectedPaths = Just $ "foo" :| ["bar"]

list :: TestTree
list = testCase "Parses list" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just () @=? cmd ^? _List
  where
    argList = ["l"]

metadata :: TestTree
metadata = testCase "Parses metadata" $ do
  (cfg, cmd) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Just () @=? cmd ^? _Metadata
  where
    argList = ["m"]
