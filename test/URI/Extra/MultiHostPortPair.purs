module Test.URI.Extra.MultiHostPortPair where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Test.Spec (Spec, describe)
import Test.Util (nes, testIso)
import URI.Authority (Authority(..), Host(..), Port, UserInfo)
import URI.Authority as Authority
import URI.Extra.MultiHostPortPair (MultiHostPortPair)
import URI.Extra.MultiHostPortPair as MultiHostPortPair
import URI.Host.IPv4Address as IPv4Address
import URI.Host.RegName as RegName
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme
import URI.URIRef (Fragment, HierPath, HierarchicalPart(..), Path(..), Query, RelPath, URI(..), URIRefOptions)
import URI.URIRef as URIRef
import URI.UserInfo as UserInfo

spec ∷ ∀ eff. Spec eff Unit
spec = do
  describe "Authority+MultiHostPortPair parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1,mongo-2"
      (Authority
        Nothing
        [ This (NameAddress (RegName.unsafeFromString $ nes "mongo-1"))
        , This (NameAddress (RegName.unsafeFromString $ nes "mongo-2"))
        ])
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1:2000,mongo-2:3000"
      (Authority
        Nothing
        [ Both (NameAddress (RegName.unsafeFromString $ nes "mongo-1")) (Port.unsafeFromInt 2000)
        , Both (NameAddress (RegName.unsafeFromString $ nes "mongo-2")) (Port.unsafeFromInt 3000)
        ])
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1:2000,mongo-2"
      (Authority
        Nothing
        [ Both (NameAddress (RegName.unsafeFromString $ nes "mongo-1")) (Port.unsafeFromInt 2000)
        , This (NameAddress (RegName.unsafeFromString $ nes "mongo-2"))
        ])
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//mongo-1,mongo-2:3000"
      (Authority
        Nothing
        [ This (NameAddress (RegName.unsafeFromString $ nes "mongo-1"))
        , Both (NameAddress (RegName.unsafeFromString $ nes "mongo-2")) (Port.unsafeFromInt 3000)
        ])
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//:2000,:3000"
      (Authority
        Nothing
        [ That (Port.unsafeFromInt 2000)
        , That (Port.unsafeFromInt 3000)
        ])
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@mongo-1,mongo-2"
      (Authority
        (Just (UserInfo.unsafeFromString (nes "user")))
        [ This (NameAddress (RegName.unsafeFromString $ nes "mongo-1"))
        , This (NameAddress (RegName.unsafeFromString $ nes "mongo-2"))
        ])
  describe "URIRef+MultiHostPortPair parser/printer" do
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Scheme.unsafeFromString "mongodb")
          (HierarchicalPartAuth
            (Authority
              (Just (UserInfo.unsafeFromString (nes "foo:bar")))
              [ This (NameAddress (RegName.unsafeFromString $ nes "db1.example.net"))
              , Both (NameAddress (RegName.unsafeFromString $ nes "db2.example.net")) (Port.unsafeFromInt 2500)
              ])
            (path ["authdb"]))
          (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
          Nothing))
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Scheme.unsafeFromString "mongodb")
          (HierarchicalPartAuth
            (Authority
              (Just (UserInfo.unsafeFromString (nes "foo:bar")))
              [ Both (NameAddress (RegName.unsafeFromString $ nes "db1.example.net")) (Port.unsafeFromInt 6)
              , Both (NameAddress (RegName.unsafeFromString $ nes "db2.example.net")) (Port.unsafeFromInt 2500)
              ])
            (path ["authdb"]))
          (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
          Nothing))
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://192.168.0.1,192.168.0.2"
      (Left
        (URI
          (Scheme.unsafeFromString "mongodb")
          (HierarchicalPartAuth
            (Authority
              Nothing
              [ This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1))
              , This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 2))
              ])
            Nothing)
          Nothing
          Nothing))

path ∷ Array String → Maybe Path
path = Just <<< Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (URIRefOptions UserInfo (MultiHostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: MultiHostPortPair.parser pure pure
  , printHosts: MultiHostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }
