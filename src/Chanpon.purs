module Chanpon where

import Prelude

import Chanpon.Classes (class FromResultFields, class Keys, class PrepareInput, class PrepareSpec, class ToParam, getFields, keys, prepareInputParams, prepareSpec, toParam)
import Data.Array as Array
import Data.List (intercalate, (:))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Foreign (F, Foreign)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Record
import Record.Builder as Builder
import SQLite3 as SQL
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Table = Table String
derive instance newtypeTable :: Newtype Table _

selectAll
  :: forall result rl
   . RowToList result rl
  => Keys rl
  => FromResultFields rl () result
  => Table
  -> SQL.DBConnection
  -> Aff (Array (F { | result }))
selectAll (Table table) db
    = map fromResultRow <<< asArray
  <$> SQL.queryDB db query mempty
  where
    columns = keys (RLProxy :: RLProxy rl)
    queryParts
      = "SELECT"
      : intercalate ", " columns
      : "FROM"
      : table
      : mempty
    query = intercalate " " queryParts <> ";"
    asArray f = unsafeCoerce f :: Array Foreign

createTableIfNotExists
  :: forall spec rl
   . RowToList spec rl
  => PrepareSpec rl spec
  => Table
  -> SQL.DBConnection
  -> { | spec }
  -> Aff Unit
createTableIfNotExists (Table table) db spec =
  void $ SQL.queryDB db query mempty
  where
    spec' = prepareSpec (RLProxy :: RLProxy rl) spec
    queryParts
      = "CREATE TABLE IF NOT EXISTS"
      : table
      : ("(" <> intercalate ", " spec' <> ")")
      : mempty
    query = intercalate " " queryParts <> ";"

insertOrReplaceInto
  :: forall input inputL
   . RowToList input inputL
  => PrepareInput inputL input
  => Table
  -> SQL.DBConnection
  -> { | input }
  -> Aff Unit
insertOrReplaceInto (Table table) db input =
  void $ SQL.queryDB db query (Array.fromFoldable params.args)
  where
    params = prepareInputParams (RLProxy :: RLProxy inputL) input 1
    queryParts
      = "INSERT OR REPLACE INTO"
      : table
      : ("(" <> intercalate ", " params.columns <> ")")
      : "VALUES"
      : ("(" <> intercalate ", " params.params <> ")")
      : mempty
    query = intercalate " " queryParts <> ";"

deleteFrom
  :: forall name ty input
   . RowToList input (Cons name ty Nil)
  => Row.Cons name ty () input
  => IsSymbol name
  => ToParam ty
  => Table
  -> SQL.DBConnection
  -> { | input }
  -> Aff Unit
deleteFrom (Table table) db input =
  void $ SQL.queryDB db query (pure $ toParam value)
  where
    nameP = SProxy :: SProxy name
    value = Record.get nameP input
    queryParts
      = "DELETE FROM"
      : table
      : "WHERE"
      : (reflectSymbol nameP)
      : "= $1"
      : mempty
    query = intercalate " " queryParts <> ";"

fromResultRow
  :: forall fields rl
   . RowToList fields rl
  => FromResultFields rl () fields
  => Foreign
  -> F { | fields }
fromResultRow o = buildRecord <$> fields
  where
    fields = getFields (RLProxy :: RLProxy rl) o
    buildRecord builder = Builder.build builder {}
