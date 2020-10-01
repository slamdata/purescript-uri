module URI.RelativePart
  ( RelativePart(..)
  , RelativePartOptions
  , RelativePartParseOptions
  , RelativePartPrintOptions
  , RelPath
  , parser
  , print
  , _authority
  , _path
  , _relPath
  , module URI.Authority
  , module URI.Path
  , module URI.Path.Absolute
  , module URI.Path.NoScheme
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
import URI.Path (Path)
import URI.Path as Path
import URI.Path.Absolute (PathAbsolute)
import URI.Path.Absolute as PathAbs
import URI.Path.NoScheme (PathNoScheme)
import URI.Path.NoScheme as PathNoScheme

-- | The "relative part" of a relative reference.  This combines an authority
-- | (optional) with a path value.
-- |
-- | When the authority is present a generic path representation can be used,
-- | otherwise there are some restrictions on the path construction to ensure
-- | no ambiguity in parsing (this is per the spec, not a restriction of the
-- | library).
data RelativePart userInfo hosts path relPath
  = RelativePartAuth (Authority userInfo hosts) path
  | RelativePartNoAuth (Maybe relPath)

derive instance eqRelativePart ∷ (Eq userInfo, Eq hosts, Eq path, Eq relPath) ⇒ Eq (RelativePart userInfo hosts path relPath)
derive instance ordRelativePart ∷ (Ord userInfo, Ord hosts, Ord path, Ord relPath) ⇒ Ord (RelativePart userInfo hosts path relPath)
derive instance genericRelativePart ∷ Generic (RelativePart userInfo hosts path relPath) _
instance showRelativePart ∷ (Show userInfo, Show hosts, Show path, Show relPath) ⇒ Show (RelativePart userInfo hosts path relPath) where show = genericShow

-- | A row type for describing the options fields used by the relative-part
-- | parser and printer.
-- |
-- | Used as `Record (RelativePartOptions userInfo hosts path relPath)`
-- | when type annotating an options record.
type RelativePartOptions userInfo hosts path relPath =
  RelativePartParseOptions userInfo hosts path relPath
    (RelativePartPrintOptions userInfo hosts path relPath ())

-- | A row type for describing the options fields used by the relative-part
-- | parser.
-- |
-- | Used as `Record (RelativePartParseOptions userInfo hosts path relPath ())`
-- | when type annotating an options record.
type RelativePartParseOptions userInfo hosts path relPath r =
  ( parseUserInfo ∷ UserInfo → Either URIPartParseError userInfo
  , parseHosts ∷ Parser String hosts
  , parsePath ∷ Path → Either URIPartParseError path
  , parseRelPath ∷ RelPath → Either URIPartParseError relPath
  | r
  )

-- | A row type for describing the options fields used by the relative-part
-- | printer.
-- |
-- | Used as `Record (RelativePartPrintOptions userInfo hosts path relPath ())`
-- | when type annotating an options record.
type RelativePartPrintOptions userInfo hosts path relPath r =
  ( printUserInfo ∷ userInfo → UserInfo
  , printHosts ∷ hosts → String
  , printPath ∷ path → Path
  , printRelPath ∷ relPath → RelPath
  | r
  )

-- | The specific path types supported in a relative-part when there is no
-- | authority present. See [`URI.Path.Absolute`](../URI.Path.Absolute) and
-- | [`URI.Path.PathNoScheme`](../URI.Path.PathNoScheme) for an explanation of
-- | these forms.
type RelPath = Either PathAbsolute PathNoScheme

-- | A parser for the relative-part of a URI.
parser
  ∷ ∀ userInfo hosts path relPath r
  . Record (RelativePartParseOptions userInfo hosts path relPath r)
  → Parser String (RelativePart userInfo hosts path relPath)
parser opts = withAuth <|> withoutAuth
  where
  withAuth =
    RelativePartAuth
      <$> Authority.parser opts
      <*> wrapParser opts.parsePath Path.parser
  withoutAuth =
    RelativePartNoAuth <$> noAuthPath
  noAuthPath
    = (Just <$> wrapParser (opts.parseRelPath <<< Left) PathAbs.parse)
    <|> (Just <$> wrapParser (opts.parseRelPath <<< Right) PathNoScheme.parse)
    <|> pure Nothing

-- | A printer for the relative-part of a URI.
print
  ∷ ∀ userInfo hosts path relPath r
  . Record (RelativePartPrintOptions userInfo hosts path relPath r)
  → RelativePart userInfo hosts path relPath → String
print opts = case _ of
  RelativePartAuth a p →
    Authority.print opts a <> Path.print (opts.printPath p)
  RelativePartNoAuth p →
    maybe "" (either PathAbs.print PathNoScheme.print <<< opts.printRelPath) p

-- | An affine traversal for the authority component of a relative-part.
_authority
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      (Authority userInfo hosts)
_authority = wander \f → case _ of
  RelativePartAuth a p → flip RelativePartAuth p <$> f a
  a → pure a

-- | An affine traversal for the path component of a relative-part, this
-- | succeeds when the authority is present also.
_path
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      path
_path = wander \f → case _ of
  RelativePartAuth a p → RelativePartAuth a <$> f p
  a → pure a

-- | An affine traversal for the path component of a relative-part, this
-- | succeeds when the authority is not present.
_relPath
  ∷ ∀ userInfo hosts path relPath
  . Traversal'
      (RelativePart userInfo hosts path relPath)
      (Maybe relPath)
_relPath = wander \f a → case a of
  RelativePartNoAuth p → RelativePartNoAuth <$> f p
  _ → pure a
