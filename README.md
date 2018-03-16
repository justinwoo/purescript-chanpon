# PureScript Chanpon

[![Build Status](https://travis-ci.org/justinwoo/purescript-chanpon.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-chanpon)

Fancy record-based queries for my [node-sqlite3](https://github.com/justinwoo/purescript-node-sqlite3) library.

![](https://i.imgur.com/jV0zhQH.png)

## Example

From [test/Main.purs](test/Main.purs)

```hs
main :: _
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
      namesOnly
         <- sequence
        <$> C.selectAll table db
              (Proxy :: Proxy { name :: String })
      withWhatever
         <- sequence
        <$> C.selectAll table db
              (Proxy :: Proxy { name :: String, whatever :: String })
      case runExcept $ Tuple <$> namesOnly <*> withWhatever of
        Right (Tuple [l] [r]) -> do
          Assert.assert "expected value from names" $ l.name == "hello"
          Assert.assert "expected value from withWhatever" $ l.name == "hello"
          Assert.assert "expected value from withWhatever" $ r.whatever == "world"
        Right a -> do
          failure $ "incorrect results: " <> unsafeCoerce a
        Left e -> do
          failure $ show e
      C.deleteFrom table db { name: "hello" }
      result <- C.selectAll table db (Proxy :: Proxy { name :: String })
      Assert.assert "table is empty after deleting" $ Array.length result == 0
  where
    testDB = "./test/test.db"
    table = C.Table "test"
```
