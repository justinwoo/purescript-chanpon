module Test.Main where

import Prelude

import Chanpon as C
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Foreign (F)
import Node.FS.Aff as FS
import SQLite3 (newDB)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = runTest do
  suite "Chanpon" do
    test "Works as expected" do
      _ <- attempt $ FS.unlink testDB
      db <- newDB testDB
      C.createTableIfNotExists table db
        { name: "text primary key unique"
        , whatever: "text"
        }
      C.insertOrReplaceInto table db
        { name: "hello"
        , whatever: "world"
        }
      namesOnly <- sequence <$> C.selectAll table db
      withWhatever <- sequence <$> C.selectAll table db
      case runExcept $ Tuple <$> namesOnly <*> withWhatever of
        Right (Tuple left right)
          | [l :: {name :: String}] <- left
          , [r :: {name :: String, whatever :: String}] <- right -> do
            Assert.assert "expected value from names" $ l.name == "hello"
            Assert.assert "expected value from withWhatever" $ l.name == "hello"
            Assert.assert "expected value from withWhatever" $ r.whatever == "world"
        Right a -> do
          failure $ "incorrect results: " <> unsafeCoerce a
        Left e -> do
          failure $ show e
      C.deleteFrom table db { name: "hello" }
      result :: Array (F {name :: String})<- C.selectAll table db
      Assert.assert "table is empty after deleting" $ Array.length result == 0
  where
    testDB = "./test/test.db"
    table = C.Table "test"
