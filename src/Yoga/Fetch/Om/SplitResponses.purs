module Yoga.Fetch.Om.SplitResponses
  ( class IsSuccessStatus
  , class ExtractBody
  , class SplitResponses
  , class SplitResponsesRL
  , class DispatchResponse
  ) where

import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Yoga.HTTP.API.Route (Response)

class IsSuccessStatus (label :: Symbol) (isSuccess :: Boolean) | label -> isSuccess

instance IsSuccessStatus "ok" True
else instance IsSuccessStatus "created" True
else instance IsSuccessStatus "accepted" True
else instance IsSuccessStatus "nonAuthoritativeInformation" True
else instance IsSuccessStatus "noContent" True
else instance IsSuccessStatus "resetContent" True
else instance IsSuccessStatus "partialContent" True
else instance IsSuccessStatus "multiStatus" True
else instance IsSuccessStatus "alreadyReported" True
else instance IsSuccessStatus "imUsed" True
else instance IsSuccessStatus label False

class ExtractBody (responseType :: Type) (bodyType :: Type) | responseType -> bodyType

instance ExtractBody (Response headers body) body

else instance
  ( Row.Cons "body" body rest row
  ) =>
  ExtractBody (Record row) body

class
  SplitResponses (allRow :: Row Type) (successRow :: Row Type) (errorRow :: Row Type)
  | allRow -> successRow errorRow

instance
  ( RL.RowToList allRow rl
  , SplitResponsesRL rl successRow errorRow
  ) =>
  SplitResponses allRow successRow errorRow

class
  SplitResponsesRL (rl :: RowList Type) (successRow :: Row Type) (errorRow :: Row Type)
  | rl -> successRow errorRow

instance SplitResponsesRL RL.Nil () ()

else instance
  ( IsSuccessStatus label isSuccess
  , ExtractBody ty body
  , SplitResponsesRL tail successTail errorTail
  , DispatchResponse isSuccess label body successTail errorTail successRow errorRow
  ) =>
  SplitResponsesRL (RL.Cons label ty tail) successRow errorRow

class
  DispatchResponse
    (isSuccess :: Boolean)
    (label :: Symbol)
    (body :: Type)
    (successTail :: Row Type)
    (errorTail :: Row Type)
    (successRow :: Row Type)
    (errorRow :: Row Type)
  | isSuccess label body successTail errorTail -> successRow errorRow

instance
  ( Row.Cons label body successTail successRow
  , Row.Lacks label successTail
  ) =>
  DispatchResponse True label body successTail errorTail successRow errorTail

instance
  ( Row.Cons label body errorTail errorRow
  , Row.Lacks label errorTail
  ) =>
  DispatchResponse False label body successTail errorTail successTail errorRow
