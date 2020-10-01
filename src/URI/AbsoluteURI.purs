module URI.AbsoluteURI
  ( AbsoluteURI(..)
  , AbsoluteURIOptions
  , AbsoluteURIParseOptions
  , AbsoluteURIPrintOptions
  , parser
  , print
  , _scheme
  , _hierPart
  , _query
  , module URI.HierarchicalPart
  , module URI.Query
  , module URI.Scheme
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
import URI.HierarchicalPart (Authority, AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, HierPath, HierarchicalPart(..), HierarchicalPartOptions, HierarchicalPartParseOptions, HierarchicalPartPrintOptions, Host(..), IPv4Address, IPv6Address, Path(..), PathAbsolute(..), PathRootless(..), Port, RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _authority, _hierPath, _hosts, _path, _userInfo)
import URI.HierarchicalPart as HPart
import URI.Query (Query)
import URI.Query as Query
import URI.Scheme (Scheme)
import URI.Scheme as Scheme

-- | A strictly absolute URI. An absolute URI can still contain relative paths
-- | but is required to have a `Scheme` component.
type AbsoluteURI userInfo hosts path hierPath query =
  { scheme :: Scheme
  , hierPart :: HierarchicalPart userInfo hosts path hierPath
  , query :: Maybe query
  }

-- | A row type for describing the options fields used by the absolute URI
-- | parser and printer.
-- |
-- | Used as `Record (AbsoluteURIOptions userInfo hosts path hierPath query)`
-- | when type anotating an options record.
-- |
-- | See below for details of how to use these configuration options.
type AbsoluteURIOptions userInfo hosts path hierPath query =
  AbsoluteURIParseOptions userInfo hosts path hierPath query
    (AbsoluteURIPrintOptions userInfo hosts path hierPath query ())

-- | A row type for describing the options fields used by the absolute URI
-- | parser.
-- |
-- | Used as `Record (AbsoluteURIParseOptions userInfo hosts path hierPath query ())`
-- | when type anotating an options record.
-- |
-- | Having this options record allows custom representations to be used for
-- | the URI components. If this is not necessary, `pure` can be used for all
-- | the options aside from `parseHosts`, which will typically be
-- | `HostPortPair.parseHosts pure pure`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair parser.
type AbsoluteURIParseOptions userInfo hosts path hierPath query r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ Either PathAbsolute PathRootless → Either URIPartParseError hierPath
  , parseQuery ∷ Query → Either URIPartParseError query
  | r
  )

-- | A row type for describing the options fields used by the absolute URI
-- | printer.
-- |
-- | Used as `Record (AbsoluteURIPrintOptions userInfo hosts path hierPath query ())`
-- | when type anotating an options record.
-- |
-- | As a reverse of the parse options, this specifies how to print values back
-- | from custom representations. If this is not necessary, `identity` can be used for
-- | all the options aside from `printHosts`, which will typically be
-- | `HostPortPair.printHosts identity identity`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair printer.
type AbsoluteURIPrintOptions userInfo hosts path hierPath query r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → Either PathAbsolute PathRootless
  , printQuery ∷ query → Query
  | r
  )

-- | A parser for an absolute URI.
parser
  ∷ ∀ userInfo hosts path hierPath query r
  . Record (AbsoluteURIParseOptions userInfo hosts path hierPath query r)
  → Parser String (AbsoluteURI userInfo hosts path hierPath query)
parser opts = ado
  scheme <- Scheme.parser
  hierPart <- HPart.parser opts
  query <- optionMaybe (wrapParser opts.parseQuery Query.parser)
  eof
  in { scheme, hierPart, query }

-- | A printer for an absolute URI.
print
  ∷ ∀ userInfo hosts path hierPath query r
  . Record (AbsoluteURIPrintOptions userInfo hosts path hierPath query r)
  → AbsoluteURI userInfo hosts path hierPath query
  → String
print opts { scheme, hierPart, query } =
  String.joinWith "" $ Array.catMaybes
    [ Just (Scheme.print scheme)
    , Just (HPart.print opts hierPart)
    , Query.print <<< opts.printQuery <$> query
    ]

-- | The scheme component of an absolute URI.
_scheme
  ∷ ∀ userInfo hosts path hierPath query
  . Lens'
      (AbsoluteURI userInfo hosts path hierPath query)
      Scheme
_scheme = prop (SProxy :: _ "scheme")

-- | The hierarchical-part component of an absolute URI.
_hierPart
  ∷ ∀ userInfo hosts path hierPath query
  . Lens'
      (AbsoluteURI userInfo hosts path hierPath query)
      (HierarchicalPart userInfo hosts path hierPath)
_hierPart = prop (SProxy :: _ "hierPart")

-- | The query component of an absolute URI.
_query
  ∷ ∀ userInfo hosts path hierPath query
  . Lens'
      (AbsoluteURI userInfo hosts path hierPath query)
      (Maybe query)
_query = prop (SProxy :: _ "query")
