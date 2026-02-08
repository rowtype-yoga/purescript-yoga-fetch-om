module Yoga.Fetch.Om.SplitParams
  ( class SplitParams
  , splitParams
  , class ExtractSubRecord
  , extractSubRecord
  ) where

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class SplitParams :: Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  SplitParams allParams pathParams queryParams bodyParams
  | allParams -> pathParams queryParams bodyParams where
  splitParams
    :: Record allParams
    -> { path :: Record pathParams
       , query :: Record queryParams
       , body :: Record bodyParams
       }

instance
  ( RowToList pathParams pathRL
  , RowToList queryParams queryRL
  , RowToList bodyParams bodyRL
  , ExtractSubRecord pathRL allParams pathParams
  , ExtractSubRecord queryRL allParams queryParams
  , ExtractSubRecord bodyRL allParams bodyParams
  ) =>
  SplitParams allParams pathParams queryParams bodyParams where
  splitParams allParamsRec =
    { path: extractSubRecord (Proxy :: _ pathRL) allParamsRec
    , query: extractSubRecord (Proxy :: _ queryRL) allParamsRec
    , body: extractSubRecord (Proxy :: _ bodyRL) allParamsRec
    }

class ExtractSubRecord :: RowList Type -> Row Type -> Row Type -> Constraint
class ExtractSubRecord rl source target | rl -> target where
  extractSubRecord :: Proxy rl -> Record source -> Record target

instance ExtractSubRecord RL.Nil source () where
  extractSubRecord _ _ = {}

instance
  ( IsSymbol label
  , Row.Cons label ty sourceTail source
  , Row.Cons label ty targetTail target
  , Row.Lacks label targetTail
  , ExtractSubRecord tail source targetTail
  ) =>
  ExtractSubRecord (RL.Cons label ty tail) source target where
  extractSubRecord _ sourceRec = Record.insert labelProxy value rest
    where
    labelProxy = Proxy :: _ label
    value = Record.get labelProxy sourceRec
    rest = extractSubRecord (Proxy :: _ tail) sourceRec
