module Yoga.Fetch.Om.ParseResponse
  ( class ParseResponse
  , parseResponse
  , class ParseSuccessRL
  , parseSuccessRL
  , class ParseErrorRL
  , parseErrorRL
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception as Exception
import JS.Fetch.Response (Response) as FetchResponse
import JS.Fetch.Response as Fetch
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Promise.Aff as Promise
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeMap, statusCodeFor, StatusCode(..))
import Yoga.JSON (class ReadForeign, readJSON_)
import Yoga.Om (Om, fromAff)
import Yoga.Om.Error (Exception)

class ParseResponse (errorRow :: Row Type) (successRow :: Row Type) where
  parseResponse :: FetchResponse.Response -> Om {} errorRow (Variant successRow)

instance
  ( RowToList successRow successRL
  , RowToList errorRow errorRL
  , ParseSuccessRL successRL successRow
  , ParseErrorRL errorRL errorRow
  ) =>
  ParseResponse errorRow successRow where
  parseResponse fetchResp = do
    let status = Fetch.status fetchResp
    jsonText <- Promise.toAffE (Fetch.text fetchResp) # fromAff
    if status >= 200 && status < 300 then
      parseSuccessRL (Proxy :: _ successRL) status jsonText # fromAff
    else if status >= 400 then
      parseErrorRL (Proxy :: _ errorRL) (Proxy :: _ errorRow) status jsonText
    else
      throwError (Variant.inj (Proxy :: _ "exception") (Exception.error ("Unexpected HTTP status code: " <> show status)))

class ParseSuccessRL (rl :: RowList Type) (successRow :: Row Type) | rl -> successRow where
  parseSuccessRL :: Proxy rl -> Int -> String -> Aff (Variant successRow)

instance ParseSuccessRL RL.Nil successRow where
  parseSuccessRL _ status _ =
    Aff.throwError (Exception.error ("Unexpected success status code: " <> show status))

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , ReadForeign body
  , Row.Cons label body tailRow successRow
  , Row.Lacks label tailRow
  , ParseSuccessRL tailRL successRow
  ) =>
  ParseSuccessRL (RL.Cons label body tailRL) successRow where
  parseSuccessRL _ actualStatus jsonText = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then case readJSON_ jsonText of
      Nothing -> Aff.throwError (Exception.error "JSON parse error")
      Just parsed -> pure (Variant.inj (Proxy :: _ label) parsed)
    else
      parseSuccessRL (Proxy :: _ tailRL) actualStatus jsonText

class ParseErrorRL (rl :: RowList Type) (errorRow :: Row Type) | rl -> errorRow where
  parseErrorRL
    :: forall successRow
     . Proxy rl
    -> Proxy errorRow
    -> Int
    -> String
    -> Om {} errorRow (Variant successRow)

instance ParseErrorRL RL.Nil errorRow where
  parseErrorRL _ _ status _ =
    throwError (Variant.inj (Proxy :: _ "exception") (Exception.error ("Unexpected error status code: " <> show status)))

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , ReadForeign body
  , Row.Cons label body tailRow errorRow
  , Row.Lacks label tailRow
  , Row.Cons label body tailExc (Exception errorRow)
  , ParseErrorRL tailRL errorRow
  ) =>
  ParseErrorRL (RL.Cons label body tailRL) errorRow where
  parseErrorRL _ errorProxy actualStatus jsonText = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then case readJSON_ jsonText of
      Nothing -> throwError (Variant.inj (Proxy :: _ "exception") (Exception.error "JSON parse error"))
      Just parsed -> throwError (Variant.inj (Proxy :: _ label) parsed)
    else
      parseErrorRL (Proxy :: _ tailRL) errorProxy actualStatus jsonText
