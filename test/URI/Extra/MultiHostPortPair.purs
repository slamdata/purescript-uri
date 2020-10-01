module Test.URI.Extra.MultiHostPortPair where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Authority (Host(..), Port, UserInfo)
import URI.Authority as Authority
import URI.Extra.MultiHostPortPair (MultiHostPortPair)
import URI.Extra.MultiHostPortPair as MultiHostPortPair
import URI.Host.IPv4Address as IPv4Address
import URI.Host.RegName as RegName
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme
import URI.URIRef (Fragment, HierPath, HierarchicalPart(..), Path(..), Query, RelPath, URIRefOptions)
import URI.URIRef as URIRef
import URI.UserInfo as UserInfo

spec ∷ Spec Unit
spec = do
  describe "Authority+MultiHostPortPair parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1,mongo-2"
      { userInfo: Nothing
      , hosts: [ This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-1")))
               , This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-2")))
               ]
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1:2000,mongo-2:3000"
      { userInfo: Nothing
      , hosts: [ Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-1"))) (Port.unsafeFromInt 2000)
               , Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-2"))) (Port.unsafeFromInt 3000)
               ]
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1:2000,mongo-2"
      { userInfo: Nothing
      , hosts: [ Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-1"))) (Port.unsafeFromInt 2000)
               , This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-2")))
               ]
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1,mongo-2:3000"
      { userInfo: Nothing
      , hosts: [ This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-1")))
               , Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-2"))) (Port.unsafeFromInt 3000)
               ]
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//:2000,:3000"
      { userInfo: Nothing
      , hosts: [ That (Port.unsafeFromInt 2000)
               , That (Port.unsafeFromInt 3000)
               ]
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@mongo-1,mongo-2"
      { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "user")))
      , hosts: [ This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-1")))
               , This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "mongo-2")))
               ]
      }
  describe "URIRef+MultiHostPortPair parser/printer" do
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "foo:bar")))
            , hosts: [ This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "db1.example.net")))
                     , Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "db2.example.net"))) (Port.unsafeFromInt 2500)
                     ]
            }
          , path: path ["authdb"]
          }
        , query: Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "foo:bar")))
            , hosts: [ Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "db1.example.net"))) (Port.unsafeFromInt 6)
                     , Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "db2.example.net"))) (Port.unsafeFromInt 2500)
                     ]
            }
          , path: path ["authdb"]
          }
        , query: Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://192.168.0.1,192.168.0.2"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: [ This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1))
                     , This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 2))
                     ]
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })

path ∷ Array String → Path
path = Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (URIRefOptions UserInfo (MultiHostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: MultiHostPortPair.parser pure pure
  , printHosts: MultiHostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseRelPath: pure
  , printRelPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }
