module URI.HierarchicalPart
  ( HierarchicalPart(..)
  , HierarchicalPartOptions
  , HierarchicalPartParseOptions
  , HierarchicalPartPrintOptions
  , HierPath
  , parser
  , print
  , _authority
  , _path
  , _hierPath
  , module URI.Authority
  , module URI.Path
  , module URI.Path.Absolute
  , module URI.Path.Rootless
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Traversal', wander)
import Data.Maybe (Maybe(..), maybe)
import Text.Parsing.Parser (Parser)
import URI.Authority (Authority, AuthorityOptions, AuthorityParseOptions, AuthorityPrintOptions, Host(..), IPv4Address, IPv6Address, Port, RegName, UserInfo, _IPv4Address, _IPv6Address, _NameAddress, _hosts, _userInfo)
import URI.Authority as Authority
import URI.Common (URIPartParseError, wrapParser)
import URI.Path (Path(..))
import URI.Path as Path
import URI.Path.Absolute (PathAbsolute(..))
import URI.Path.Absolute as PathAbs
import URI.Path.Rootless (PathRootless(..))
import URI.Path.Rootless as PathRootless

-- | The "hierarchical part" of a generic or absolute URI. This combines an
-- | authority (optional) with a path value.
-- |
-- | When the authority is present a generic path representation can be used,
-- | otherwise there are some restrictions on the path construction to ensure
-- | no ambiguity in parsing (this is per the spec, not a restriction of the
-- | library).
data HierarchicalPart userInfo hosts path hierPath
  = HierarchicalPartAuth { authority :: Authority userInfo hosts
                         , path :: path
                         }
  | HierarchicalPartNoAuth (Maybe hierPath)

derive instance eqHierarchicalPart ∷ (Eq userInfo, Eq hosts, Eq path, Eq hierPath) ⇒ Eq (HierarchicalPart userInfo hosts path hierPath)
derive instance ordHierarchicalPart ∷ (Ord userInfo, Ord hosts, Ord path, Ord hierPath) ⇒ Ord (HierarchicalPart userInfo hosts path hierPath)
derive instance genericHierarchicalPart ∷ Generic (HierarchicalPart userInfo hosts path hierPath) _
instance showHierarchicalPart ∷ (Show userInfo, Show hosts, Show path, Show hierPath) ⇒ Show (HierarchicalPart userInfo hosts path hierPath) where show = genericShow

-- | A row type for describing the options fields used by the hierarchical-part
-- | parser and printer.
-- |
-- | Used as `Record (HierarchicalPartOptions userInfo hosts path hierPath)`
-- | when type anotating an options record.
type HierarchicalPartOptions userInfo hosts path hierPath =
  HierarchicalPartParseOptions userInfo hosts path hierPath
    (HierarchicalPartPrintOptions userInfo hosts path hierPath ())

-- | A row type for describing the options fields used by the hierarchical-part
-- | parser.
-- |
-- | Used as `Record (HierarchicalPartParseOptions userInfo hosts path hierPath ())`
-- | when type anotating an options record.
type HierarchicalPartParseOptions userInfo hosts path hierPath r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseHierPath ∷ HierPath → Either URIPartParseError hierPath
  | r
  )

-- | A row type for describing the options fields used by the hierarchical-part
-- | printer.
-- |
-- | Used as `Record (HierarchicalPartPrintOptions userInfo hosts path hierPath ())`
-- | when type anotating an options record.
type HierarchicalPartPrintOptions userInfo hosts path hierPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printHierPath ∷ hierPath → HierPath
  | r
  )

-- | The specific path types supported in a hierarchical-part when there is no
-- | authority present. See [`URI.Path.Absolute`](../URI.Path.Absolute) and
-- | [`URI.Path.Rootless`](../URI.Path.Rootless) for an explanation of these
-- | forms.
type HierPath = Either PathAbsolute PathRootless

-- | A parser for the hierarchical-part of a URI.
parser
  ∷ ∀ userInfo hosts path hierPath r
  . Record (HierarchicalPartParseOptions userInfo hosts path hierPath r)
  → Parser String (HierarchicalPart userInfo hosts path hierPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth = ado
    authority <- Authority.parser opts
    path <- wrapParser opts.parsePath Path.parser
    in HierarchicalPartAuth { authority, path }
  withoutAuth =
    HierarchicalPartNoAuth <$> noAuthPath
  noAuthPath
    = (Just <$> wrapParser (opts.parseHierPath <<< Left) PathAbs.parse)
    <|> (Just <$> wrapParser (opts.parseHierPath <<< Right) PathRootless.parse)
    <|> pure Nothing

-- | A printer for the hierarchical-part of a URI.
print
  ∷ ∀ userInfo hosts path hierPath r
  . Record (HierarchicalPartPrintOptions userInfo hosts path hierPath r)
  → HierarchicalPart userInfo hosts path hierPath → String
print opts = case _ of
  HierarchicalPartAuth { authority, path } →
    Authority.print opts authority <> Path.print (opts.printPath path)
  HierarchicalPartNoAuth p →
    maybe "" (either PathAbs.print PathRootless.print <<< opts.printHierPath) p

-- | An affine traversal for the authority component of a hierarchical-part.
_authority
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      (Authority userInfo hosts)
_authority = wander \f → case _ of
  HierarchicalPartAuth rec → f rec.authority <#> \a -> (HierarchicalPartAuth (rec { authority = a }))
  a → pure a

-- | An affine traversal for the path component of a hierarchical-part, this
-- | succeeds when the authority is present also.
_path
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      path
_path = wander \f → case _ of
  HierarchicalPartAuth rec → f rec.path <#> \p -> (HierarchicalPartAuth (rec { path = p }))
  a → pure a

-- | An affine traversal for the path component of a hierarchical-part, this
-- | succeeds when the authority is not present.
_hierPath
  ∷ ∀ userInfo hosts path hierPath
  . Traversal'
      (HierarchicalPart userInfo hosts path hierPath)
      (Maybe hierPath)
_hierPath = wander \f → case _ of
  HierarchicalPartNoAuth p → HierarchicalPartNoAuth <$> f p
  a → pure a
