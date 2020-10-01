module URI.URIRef
  ( URIRef
  , URIRefOptions
  , URIRefParseOptions
  , URIRefPrintOptions
  , parser
  , print
  , module URI.Authority
  , module URI.Fragment
  , module URI.Host
  , module URI.Path
  , module URI.Path.Absolute
  , module URI.Path.NoScheme
  , module URI.Path.Rootless
  , module URI.Port
  , module URI.Query
  , module URI.RelativeRef
  , module URI.Scheme
  , module URI.URI
  , module URI.UserInfo
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import URI.Authority (Authority)
import URI.Common (URIPartParseError)
import URI.Fragment (Fragment)
import URI.Host (Host(..), IPv4Address, IPv6Address, RegName, _IPv4Address, _IPv6Address, _NameAddress)
import URI.Path (Path(..))
import URI.Path.Absolute (PathAbsolute(..))
import URI.Path.NoScheme (PathNoScheme(..))
import URI.Path.Rootless (PathRootless(..))
import URI.Port (Port)
import URI.Query (Query)
import URI.RelativeRef (RelPath, RelativePart(..), RelativeRef)
import URI.RelativeRef as RelativeRef
import URI.Scheme (Scheme)
import URI.URI (HierPath, HierarchicalPart(..), URI)
import URI.URI as URI
import URI.UserInfo (UserInfo)

-- | The most general kind of URI, can either be relative or absolute.
type URIRef userInfo hosts path hierPath relPath query fragment =
  Either
    (URI.URI userInfo hosts path hierPath query fragment)
    (RelativeRef.RelativeRef userInfo hosts path relPath query fragment)

-- | A row type for describing the options fields used by the general URI
-- | parser and printer.
-- |
-- | Used as `Record (URIRefOptions userInfo hosts path hierPath relPath query fragment)`
-- | when type anotating an options record.
-- |
-- | See below for details of how to use these configuration options.
type URIRefOptions userInfo hosts path hierPath relPath query fragment =
  URIRefParseOptions userInfo hosts path hierPath relPath query fragment
    (URIRefPrintOptions userInfo hosts path hierPath relPath query fragment ())

-- | A row type for describing the options fields used by the general URI
-- | parser.
-- |
-- | Used as `Record (URIRefParseOptions userInfo hosts path hierPath relPath query fragment ())`
-- | when type anotating an options record.
-- |
-- | Having this options record allows custom representations to be used for
-- | the URI components. If this is not necessary, `pure` can be used for all
-- | the options aside from `parseHosts`, which will typically be
-- | `HostPortPair.parseHosts pure pure`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair parser.
type URIRefParseOptions userInfo hosts path hierPath relPath query fragment r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ Either PathAbsolute PathRootless → Either URIPartParseError hierPath
  , parseRelPath ∷ Either PathAbsolute PathNoScheme → Either URIPartParseError relPath
  , parseQuery ∷ Query → Either URIPartParseError query
  , parseFragment ∷ Fragment → Either URIPartParseError fragment
  | r
  )

-- | A row type for describing the options fields used by the general URI
-- | printer.
-- |
-- | Used as `Record (URIRefPrintOptions userInfo hosts path hierPath relPath query fragment ())`
-- | when type anotating an options record.
-- |
-- | As a reverse of the parse options, this specifies how to print values back
-- | from custom representations. If this is not necessary, `id` can be used for
-- | all the options aside from `printHosts`, which will typically be
-- | `HostPortPair.printHosts id id`. See [`URI.HostPortPair`](../URI.HostPortPair)
-- | for more information on the host/port pair printer.
type URIRefPrintOptions userInfo hosts path hierPath relPath query fragment r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → Either PathAbsolute PathRootless
  , printRelPath ∷ relPath → Either PathAbsolute PathNoScheme
  , printQuery ∷ query → Query
  , printFragment ∷ fragment → Fragment
  | r
  )

-- | A parser for a general URI.
parser
  ∷ ∀ userInfo hosts path hierPath relPath query fragment r
  . Record (URIRefParseOptions userInfo hosts path hierPath relPath query fragment r)
  → Parser String (URIRef userInfo hosts path hierPath relPath query fragment)
parser opts
  = try (Left <$> URI.parser opts)
  <|> (Right <$> RelativeRef.parser opts)

-- | A printer for a general URI.
print
  ∷ ∀ userInfo hosts path hierPath relPath query fragment r
  . Record (URIRefPrintOptions userInfo hosts path hierPath relPath query fragment r)
  → URIRef userInfo hosts path hierPath relPath query fragment
  → String
print opts = either (URI.print opts) (RelativeRef.print opts)
