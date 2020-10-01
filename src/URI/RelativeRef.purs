module URI.RelativeRef
  ( RelativeRef(..)
  , RelativeRefOptions
  , RelativeRefParseOptions
  , RelativeRefPrintOptions
  , parser
  , print
  , _relPart
  , _query
  , _fragment
  , module URI.Fragment
  , module URI.Query
  , module URI.RelativePart
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (eof)
import URI.Common (URIPartParseError, wrapParser)
import URI.Fragment (Fragment)
import URI.Fragment as Fragment
import URI.Query (Query)
import URI.Query as Query
import URI.RelativePart (Authority, AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, Host(..), IPv4Address, IPv6Address, Path, PathAbsolute, PathNoScheme, Port, RegName, RelPath, RelativePart(..), RelativePartOptions, RelativePartParseOptions, RelativePartPrintOptions, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hosts, _path, _relPath, _userInfo)
import URI.RelativePart as RPart

-- | A relative URI. Relative in the sense that it lacks a `Scheme` component.
type RelativeRef userInfo hosts path relPath query fragment =
  { relPart :: RelativePart userInfo hosts path relPath
  , query :: Maybe query
  , fragment :: Maybe fragment
  }

-- | A row type for describing the options fields used by the relative URI
-- | parser and printer.
-- |
-- | Used as `Record (RelativeRefOptions userInfo hosts path relPath query fragment)`
-- | when type anotating an options record.
-- |
-- | See below for details of how to use these configuration options.
type RelativeRefOptions userInfo hosts path relPath query fragment =
  RelativeRefParseOptions userInfo hosts path relPath query fragment
    (RelativeRefPrintOptions userInfo hosts path relPath query fragment ())

-- | A row type for describing the options fields used by the relative URI
-- | parser.
-- |
-- | Used as `Record (RelativeRefParseOptions userInfo hosts path relPath query fragment ())`
-- | when type anotating an options record.
-- |
-- | Having this options record allows custom representations to be used for
-- | the URI components. If this is not necessary, `pure` can be used for all
-- | the options aside from `parseHosts`, which will typically be
-- | `HostPortPair.parseHosts pure pure`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair parser.
type RelativeRefParseOptions userInfo hosts path relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseRelPath ∷ Either PathAbsolute PathNoScheme → Either URIPartParseError relPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

-- | A row type for describing the options fields used by the relative URI
-- | printer.
-- |
-- | As a reverse of the parse options, this specifies how to print values back
-- | from custom representations. If this is not necessary, `identity` can be used for
-- | all the options aside from `printHosts`, which will typically be
-- | `HostPortPair.printHosts identity identity`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair printer.
type RelativeRefPrintOptions userInfo hosts path relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printRelPath ∷ relPath → Either PathAbsolute PathNoScheme
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

-- | A parser for a relative URI.
parser
  ∷ ∀ userInfo hosts path relPath query fragment r
  . Record (RelativeRefParseOptions userInfo hosts path relPath query fragment r)
  → Parser String (RelativeRef userInfo hosts path relPath query fragment)
parser opts = ado
  relPart <- RPart.parser opts
  query <- optionMaybe (wrapParser opts.parseQuery Query.parser)
  fragment <- optionMaybe (wrapParser opts.parseFragment Fragment.parser)
  eof
  in { relPart, query, fragment }

-- | A printer for a relative URI.
print
  ∷ ∀ userInfo hosts path relPath query fragment r
  . Record (RelativeRefPrintOptions userInfo hosts path relPath query fragment r)
  → RelativeRef userInfo hosts path relPath query fragment
  → String
print opts { relPart, query, fragment } =
  String.joinWith "" $ Array.catMaybes
    [ Just (RPart.print opts relPart)
    , Query.print <<< opts.printQuery <$> query
    , Fragment.print <<< opts.printFragment <$> fragment
    ]

-- | The relative-part component of a relative URI.
_relPart
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (RelativePart userInfo hosts path relPath)
_relPart = prop (SProxy :: _ "relPart")

-- | The query component of a relative URI.
_query
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (Maybe query)
_query = prop (SProxy :: _ "query")

-- | The fragment component of a relative URI.
_fragment
  ∷ ∀ userInfo hosts path relPath query fragment
  . Lens'
      (RelativeRef userInfo hosts path relPath query fragment)
      (Maybe fragment)
_fragment = prop (SProxy :: _ "fragment")
