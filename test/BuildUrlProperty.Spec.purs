module BuildUrlProperty.Spec where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Enum (toEnum)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Gen (genAsciiString)
import Data.String.CodePoints (CodePoint, fromCodePointArray)
import Data.String.Pattern (Pattern(..))
import JSURI (decodeURIComponent)
import Test.QuickCheck ((<?>), (===))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf1, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Fetch.Om.BuildUrl (appendQueryParams, substitutePathParams)

spec :: Spec Unit
spec = describe "URL properties" do
  it "never substitutes path captures in the query or fragment" $ quickCheck do
    value <- (genAsciiString :: Gen String)
    let
      suffix = "?next=/:slug#/:slug"
      result = substitutePathParams @(slug :: String)
        { slug: value }
        ("/items/:slug" <> suffix)
      property = case String.stripSuffix (Pattern suffix) result of
        Nothing -> false
        Just path -> not $ String.contains (Pattern ":slug") path
    pure $ property <?> "unexpected path substitution result: " <> show result

  it "round-trips percent-encoded path and query values" $ quickCheck do
    value <- (genAsciiString :: Gen String)
    let
      pathResult = substitutePathParams @(value :: String)
        { value }
        "/items/:value"
      queryResult = appendQueryParams @(value :: String)
        { value }
        "/items"
      property = case
        String.stripPrefix (Pattern "/items/") pathResult,
        String.stripPrefix (Pattern "/items?value=") queryResult
        of
        Just encodedPath, Just encodedQuery ->
          decodeURIComponent encodedPath == Just value
            && decodeURIComponent encodedQuery == Just value
        _, _ -> false
    pure $ property <?> "failed to round-trip value " <> show value

  it "preserves existing query values and fragments" $ quickCheck do
    existing <- (genAsciiString :: Gen String)
    added <- (genAsciiString :: Gen String)
    let
      base = appendQueryParams @(token :: String)
        { token: existing }
        "/items#details"
      result = appendQueryParams @(search :: String)
        { search: added }
        base
      property = do
        withoutFragment <- String.stripSuffix (Pattern "#details") result
        query <- String.stripPrefix (Pattern "/items?token=") withoutFragment
        case String.split (Pattern "&search=") query of
          [ encodedExisting, encodedAdded ] ->
            pure
              $ decodeURIComponent encodedExisting == Just existing
              && decodeURIComponent encodedAdded == Just added
          _ -> Nothing
    pure
      $ (property == Just true)
      <?> "query or fragment changed unexpectedly: " <> show result

  it "preserves repeated query parameter order and multiplicity" $ quickCheck do
    values <- arbitrary
    let
      result = appendQueryParams @(tag :: Array Int) { tag: values } "/items"
      expected = case values of
        [] -> "/items"
        _ -> "/items?" <> String.joinWith "&" (map (\value -> "tag=" <> show value) values)
    pure $ result === expected

  it "normalizes absent, Nothing, and Just optional fields" $ quickCheck do
    missing <- arbitrary
    value <- arbitrary
    let
      params :: Record (id :: Maybe Int)
      params = if missing then unsafeCoerce {} else { id: value }
      result = appendQueryParams @(id :: Maybe Int) params "/items"
      expected = case missing, value of
        true, _ -> "/items"
        false, Nothing -> "/items"
        false, Just id -> "/items?id=" <> show id
    pure $ result === expected

  it "never introduces malformed query separators" $ quickCheck do
    value <- arbitrary
    let
      bases =
        [ "/items"
        , "/items?existing=1"
        , "/items?"
        , "/items?existing=1&"
        , "/items#details"
        , "/items?existing=1#details"
        , "/items?#details"
        , "/items?existing=1&#details"
        ]
      results = map (appendQueryParams @(id :: Int) { id: value }) bases
    pure
      $ all isWellFormed results
      <?> "malformed URL generated from " <> show results

  it "preserves repeated existing query keys" $ quickCheck do
    first <- (arbitrary :: Gen Int)
    second <- (arbitrary :: Gen Int)
    page <- (arbitrary :: Gen Int)
    let
      base = "/items?id=" <> show first <> "&id=" <> show second <> "#details"
      result = appendQueryParams @(page :: Int) { page } base
      expected =
        "/items?id=" <> show first <> "&id=" <> show second
          <> "&page="
          <> show page
          <> "#details"
    pure $ result === expected

  it "appends duplicate query values without coalescing existing ones" $ quickCheck do
    first <- (arbitrary :: Gen Int)
    second <- (arbitrary :: Gen Int)
    added <- (arbitrary :: Gen (Array Int))
    let
      base = "/items?id=" <> show first <> "&id=" <> show second
      result = appendQueryParams @(id :: Array Int) { id: added } base
      expected = case added of
        [] -> base
        _ -> base <> "&" <> String.joinWith "&" (map (\value -> "id=" <> show value) added)
    pure $ result === expected

  it "ignores query-looking text inside fragments" $ quickCheck do
    value <- (arbitrary :: Gen Int)
    let
      rendered = show (value :: Int)
      cases =
        [ { base: "/items#section?fake=1", expected: "/items?id=" <> rendered <> "#section?fake=1" }
        , { base: "/items?real=1#section?fake=1", expected: "/items?real=1&id=" <> rendered <> "#section?fake=1" }
        , { base: "/items#?fake=1&other=2", expected: "/items?id=" <> rendered <> "#?fake=1&other=2" }
        , { base: "/items#first#second?fake=1", expected: "/items?id=" <> rendered <> "#first#second?fake=1" }
        ]
      property = all
        (\entry -> appendQueryParams @(id :: Int) { id: value } entry.base == entry.expected)
        cases
    pure $ property <?> "fragment handling failed for id " <> rendered

  it "encodes every query delimiter inside values" $ quickCheck do
    prefix <- (genAsciiString :: Gen String)
    suffix <- (genAsciiString :: Gen String)
    let
      value = prefix <> "?#&=%+ /" <> suffix
      result = appendQueryParams @(value :: String) { value } "/items"
      property = case String.stripPrefix (Pattern "/items?value=") result of
        Nothing -> false
        Just encoded ->
          decodeURIComponent encoded == Just value
            && all
              (\delimiter -> not $ String.contains (Pattern delimiter) encoded)
              [ "?", "#", "&", "=", "+", "/" ]
            && String.contains (Pattern "%25") encoded
    pure $ property <?> "delimiter encoding failed: " <> show result

  it "substitutes repeated captures without matching overlapping names" $ quickCheck do
    id <- (arbitrary :: Gen Int)
    id2 <- (arbitrary :: Gen Int)
    let
      result = substitutePathParams @(id :: Int, id2 :: Int)
        { id, id2 }
        "/:id/:id/:id2/:id20?next=/:id#/:id2"
      expected =
        "/" <> show id <> "/" <> show id <> "/" <> show id2
          <> "/:id20?next=/:id#/:id2"
    pure $ result === expected

  it "normalizes lone UTF-16 surrogates before percent-encoding" do
    let
      value = codeUnit 0xD800 <> "x" <> codeUnit 0xDFFF
      result = appendQueryParams @(value :: String) { value } "/items"
    result `shouldEqual` "/items?value=%EF%BF%BDx%EF%BF%BD"

  it "percent-encodes arbitrary UTF-16 strings without throwing" $ quickCheck do
    value <- (arbitrary :: Gen String)
    let
      pathResult = substitutePathParams @(value :: String) { value } "/items/:value"
      queryResult = appendQueryParams @(value :: String) { value } "/items"
      property =
        isJust (String.stripPrefix (Pattern "/items/") pathResult)
          && isJust (String.stripPrefix (Pattern "/items?value=") queryResult)
    pure $ property <?> "encoding failed for UTF-16 input " <> show value

  it "round-trips valid non-ASCII scalar values" $ quickCheck do
    value <- unicodeScalarString
    let
      pathResult = substitutePathParams @(value :: String) { value } "/items/:value"
      queryResult = appendQueryParams @(value :: String) { value } "/items"
      property = case
        String.stripPrefix (Pattern "/items/") pathResult,
        String.stripPrefix (Pattern "/items?value=") queryResult
        of
        Just encodedPath, Just encodedQuery ->
          decodeURIComponent encodedPath == Just value
            && decodeURIComponent encodedQuery == Just value
        _, _ -> false
    pure $ property <?> "Unicode round-trip failed for " <> show value

isWellFormed :: String -> Boolean
isWellFormed url =
  occurrences "?" url == 1
    && occurrences "#" url <= 1
    && not (String.contains (Pattern "??") url)
    && not (String.contains (Pattern "&&") url)
    && not (String.contains (Pattern "?#") url)
    && not (String.contains (Pattern "&#") url)
    && queryPrecedesFragment url

queryPrecedesFragment :: String -> Boolean
queryPrecedesFragment url = case String.indexOf (Pattern "#") url of
  Nothing -> true
  Just fragmentIndex -> case String.indexOf (Pattern "?") url of
    Nothing -> false
    Just queryIndex -> queryIndex < fragmentIndex

occurrences :: String -> String -> Int
occurrences needle = (_ - 1) <<< Array.length <<< String.split (Pattern needle)

unicodeScalarString :: Gen String
unicodeScalarString =
  fromCodePointArray <<< NEA.toArray <$> arrayOf1 unicodeScalar

unicodeScalar :: Gen CodePoint
unicodeScalar = do
  useBasicMultilingualPlane <- arbitrary
  value <- if useBasicMultilingualPlane then
    chooseInt 0x80 0xD7FF
  else
    chooseInt 0xE000 0x10FFFF
  pure $ unsafePartial $ fromJust $ toEnum value

codeUnit :: Int -> String
codeUnit value = CodeUnits.singleton $ unsafePartial $ fromJust $ toEnum value
