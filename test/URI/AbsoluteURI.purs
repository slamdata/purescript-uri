module Test.URI.AbsoluteURI where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.AbsoluteURI (AbsoluteURIOptions, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathRootless(..), Port, Query, UserInfo)
import URI.AbsoluteURI as AbsoluteURI
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme

spec ∷ Spec Unit
spec =
  describe "AbsoluteURI parser/printer" do
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost/testBucket?password=&docTypeKey="
      { scheme: Scheme.unsafeFromString "couchbase"
      , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
            }
          , path: path ["testBucket"]
          }
      , query: Just (Query.unsafeFromString "password=&docTypeKey=")
      }
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost:9999/testBucket?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      { scheme: Scheme.unsafeFromString "couchbase"
      , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))) (Port.unsafeFromInt 9999))
            }
          , path: path ["testBucket"]
          }
      , query: Just (Query.unsafeFromString "password=pass&docTypeKey=type&queryTimeoutSeconds=20")
      }
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:/abc/def"
      { scheme: Scheme.unsafeFromString "foo"
      , hierPart: HierarchicalPartNoAuth
          (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "abc")) [PathSegment.unsafeSegmentFromString "def"])))))
      , query: Nothing
      }
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:abc/def"
      { scheme: Scheme.unsafeFromString "foo"
      , hierPart: HierarchicalPartNoAuth
          (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "abc")) [PathSegment.unsafeSegmentFromString "def"]))))
      , query: Nothing
      }

path ∷ Array String → Path
path = Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }
