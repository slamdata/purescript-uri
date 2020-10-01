module URI
  ( module URI.AbsoluteURI
  , module URI.Authority
  , module URI.Fragment
  , module URI.HierarchicalPart
  , module URI.Host
  , module URI.Path
  , module URI.Path.Absolute
  , module URI.Path.NoScheme
  , module URI.Path.Rootless
  , module URI.Port
  , module URI.Query
  , module URI.RelativePart
  , module URI.RelativeRef
  , module URI.Scheme
  , module URI.URI
  , module URI.URIRef
  , module URI.UserInfo
  ) where

import URI.AbsoluteURI (AbsoluteURI)
import URI.Authority (Authority)
import URI.Fragment (Fragment)
import URI.HierarchicalPart (HierarchicalPart(..), HierPath)
import URI.Host (Host(..), RegName, IPv4Address, IPv6Address)
import URI.Path (Path(..))
import URI.Path.Absolute (PathAbsolute(..))
import URI.Path.NoScheme (PathNoScheme(..))
import URI.Path.Rootless (PathRootless(..))
import URI.Port (Port)
import URI.Query (Query)
import URI.RelativePart (RelativePart(..), RelPath)
import URI.RelativeRef (RelativeRef)
import URI.Scheme (Scheme)
import URI.URI (URI)
import URI.URIRef (URIRef)
import URI.UserInfo (UserInfo)
