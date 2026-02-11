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
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import JS.Fetch.Response (Response) as FetchResponse
import JS.Fetch.Response as Fetch
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Promise.Aff as Promise
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fetch.Om.StreamDecode (class StreamDecode, decodeStream)
import Yoga.HTTP.API.Route.Encoding (PlainText, Streaming)
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeMap, statusCodeFor, StatusCode(..))
import Yoga.JSON (class ReadForeign, readJSON)
import Yoga.JSON.Error (withStringErrors)
import Yoga.Om (class ToOm, Om, toOm)
import Yoga.Om.Error (Exception)
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom.WebStream as WebStream

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
    if status >= 200 && status < 300 then
      parseSuccessRL (Proxy :: _ successRL) status fetchResp # toOm
    else if status >= 400 then do
      errorText <- Promise.toAffE (Fetch.text fetchResp) # toOm
      parseErrorRL (Proxy :: _ errorRL) (Proxy :: _ errorRow) status errorText
    else
      throwError (Variant.inj (Proxy :: _ "exception") (Exception.error ("Unexpected HTTP status code: " <> show status)))

class ParseSuccessRL (rl :: RowList Type) (successRow :: Row Type) | rl -> successRow where
  parseSuccessRL :: Proxy rl -> Int -> FetchResponse.Response -> Aff (Variant successRow)

instance ParseSuccessRL RL.Nil successRow where
  parseSuccessRL _ status _ =
    Aff.throwError (Exception.error ("Unexpected success status code: " <> show status))

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , Row.Cons label PlainText tailRow successRow
  , Row.Lacks label tailRow
  , ParseSuccessRL tailRL successRow
  ) =>
  ParseSuccessRL (RL.Cons label PlainText tailRL) successRow where
  parseSuccessRL _ actualStatus fetchResp = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then do
      text <- liftEffect (Fetch.text fetchResp) >>= Promise.toAff
      pure (Variant.inj (Proxy :: _ label) (unsafeCoerce text))
    else
      parseSuccessRL (Proxy :: _ tailRL) actualStatus fetchResp

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , StreamDecode a
  , Row.Cons label (Streaming a) tailRow successRow
  , Row.Lacks label tailRow
  , ParseSuccessRL tailRL successRow
  ) =>
  ParseSuccessRL (RL.Cons label (Streaming a) tailRL) successRow where
  parseSuccessRL _ actualStatus fetchResp = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then do
      webStream <- liftEffect (Fetch.body fetchResp)
      let strom = decodeStream (WebStream.fromReadableStream (unsafeCoerce webStream)) :: Strom {} () a
      pure (Variant.inj (Proxy :: _ label) (unsafeCoerce strom))
    else
      parseSuccessRL (Proxy :: _ tailRL) actualStatus fetchResp

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , ReadForeign body
  , Row.Cons label body tailRow successRow
  , Row.Lacks label tailRow
  , ParseSuccessRL tailRL successRow
  ) =>
  ParseSuccessRL (RL.Cons label body tailRL) successRow where
  parseSuccessRL _ actualStatus fetchResp = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then do
      jsonText <- liftEffect (Fetch.text fetchResp) >>= Promise.toAff
      case withStringErrors (readJSON jsonText) of
        Left errs -> Aff.throwError (Exception.error ("Failed to parse response (status " <> show actualStatus <> "):\n" <> errs))
        Right parsed -> pure (Variant.inj (Proxy :: _ label) parsed)
    else
      parseSuccessRL (Proxy :: _ tailRL) actualStatus fetchResp

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
  , Row.Cons label PlainText tailRow errorRow
  , Row.Lacks label tailRow
  , Row.Cons label PlainText tailExc (Exception errorRow)
  , ParseErrorRL tailRL errorRow
  ) =>
  ParseErrorRL (RL.Cons label PlainText tailRL) errorRow where
  parseErrorRL _ errorProxy actualStatus text = do
    let StatusCode expected = statusCodeFor (Proxy :: _ label)
    if actualStatus == expected then
      throwError (Variant.inj (Proxy :: _ label) (unsafeCoerce text))
    else
      parseErrorRL (Proxy :: _ tailRL) errorProxy actualStatus text

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
    if actualStatus == expected then case withStringErrors (readJSON jsonText) of
      Left errs -> throwError (Variant.inj (Proxy :: _ "exception") (Exception.error ("Failed to parse error response (status " <> show actualStatus <> "):\n" <> errs)))
      Right parsed -> throwError (Variant.inj (Proxy :: _ label) parsed)
    else
      parseErrorRL (Proxy :: _ tailRL) errorProxy actualStatus jsonText
