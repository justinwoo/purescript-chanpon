module Chanpon.Classes where

import Prelude

import Control.Monad.Except (withExcept)
import Data.Foreign (F, Foreign, ForeignError(..), readInt, readString)
import Data.Foreign.Index (readProp)
import Data.List (List)
import Data.List as List
import Data.Monoid (mempty)
import Data.Record (get)
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import SQLite3 as SQL
import Type.Prelude (class IsSymbol, class RowToList, SProxy(SProxy), reflectSymbol)
import Type.Row (class RowLacks, Cons, Nil, RLProxy(..), kind RowList)

class ToParam a where
  toParam :: a -> String

instance toParamInt :: ToParam Int where
  toParam = show

instance toParamString :: ToParam String where
  toParam = id

class FromResult a where
  fromResult :: Foreign -> F a

instance fromResultInt :: FromResult Int where
  fromResult = readInt

instance fromResultString :: FromResult String where
  fromResult = readString

class FromResultFields (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  getFields :: RLProxy xs
    -> Foreign
    -> F (Builder (Record from) (Record to))

instance fromResultFields :: FromResultFields Nil () () where
  getFields _ _ = pure id

instance fromResultFieldsCons ::
  ( IsSymbol name
  , FromResult ty
  , FromResultFields tail from from'
  , RowLacks name from'
  , RowCons name ty from' to
  ) => FromResultFields (Cons name ty tail) from to where
  getFields _ obj = do
    value <- withExcept' $ fromResult =<< readProp name obj
    compose (Builder.insert nameP value) <$> getFields tailP obj
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtProperty name

class PrepareInput (xs :: RowList) (row :: # Type)
  | xs -> row where
  prepareInputParams
    :: RLProxy xs
    -> { | row}
    -> Int
    -> { columns :: List String
       , params :: List String
       , args :: List SQL.Param
       }

instance prepareInputParamsNil :: PrepareInput Nil row where
  prepareInputParams _ _ _ = { columns: mempty, params: mempty, args: mempty }

instance prepareInputParamsCons ::
  ( PrepareInput tail row
  , RowCons name ty trash row
  , IsSymbol name
  , ToParam ty
  ) => PrepareInput (Cons name ty tail) row where
  prepareInputParams _ r i = { columns, params, args }
    where
      nameP = SProxy :: SProxy name
      rest = prepareInputParams (RLProxy :: RLProxy tail) r (i + 1)
      columns = List.Cons (reflectSymbol nameP) rest.columns
      params = List.Cons ("$" <> show i) rest.params
      args = List.Cons (toParam $ get nameP r) rest.args

class PrepareSpec (xs :: RowList) (row :: # Type)
  | xs -> row where
  prepareSpec
    :: RLProxy xs
    -> { | row}
    -> List String

instance prepareSpecNil :: PrepareSpec Nil trash where
  prepareSpec _ _ = mempty

instance prepareSpecCons ::
  ( PrepareSpec tail row
  , IsSymbol name
  , RowCons name String trash row
  ) => PrepareSpec (Cons name ty tail) row where
  prepareSpec _ r = List.Cons first rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      spec = get nameP r
      first = name <> " " <> spec
      rest = prepareSpec (RLProxy :: RLProxy tail) r

class Keys (xs :: RowList) where
  keys :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keys _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keys _ = List.Cons first rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keys (RLProxy :: RLProxy tail)

keysAsRecord
  :: forall proxy row rl keys
   . RowToList row rl
  => KeysAsRecordBuilder rl () keys
  => proxy row
  -> { | keys }
keysAsRecord _ = Builder.build builder {}
  where
    builder = keysAsRecordBuilder (RLProxy :: RLProxy rl)

class KeysAsRecordBuilder (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  keysAsRecordBuilder :: RLProxy xs -> Builder (Record from) (Record to)

instance keysAsRecordBuilderNil :: KeysAsRecordBuilder Nil () () where
  keysAsRecordBuilder _ = id

instance keysAsRecordBuilderCons ::
  ( KeysAsRecordBuilder tail from from'
  , RowLacks name from'
  , RowCons name String from' to
  , IsSymbol name
  ) => KeysAsRecordBuilder (Cons name trash tail) from to where
  keysAsRecordBuilder _ = first <<< rest
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      first = Builder.insert nameP name
      rest = keysAsRecordBuilder tailP
