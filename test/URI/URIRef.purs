module Test.URI.URIRef where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Fragment as Fragment
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme
import URI.URIRef (Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathNoScheme(..), PathRootless(..), Port, Query, RelPath, RelativePart(..), URIRefOptions, UserInfo)
import URI.URIRef as URIRef
import URI.UserInfo as UserInfo

spec ∷ Spec Unit
spec =
  describe "URIRef parser/printer" do
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:///?q=foo&var.bar=baz"
      (Left
        { scheme: Scheme.unsafeFromString "sql2"
        , hierPart: HierarchicalPartAuth
            { authority: { userInfo: Nothing, hosts: Nothing }
            , path: path [""]
            }
        , query: Just (Query.unsafeFromString "q=foo&var.bar=baz")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2://?q=foo&var.bar=baz"
      (Left
        { scheme: Scheme.unsafeFromString "sql2"
        , hierPart: HierarchicalPartAuth
            { authority: { userInfo: Nothing, hosts: Nothing }
            , path: path []
            }
        , query: Just (Query.unsafeFromString "q=foo&var.bar=baz")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:/?q=foo&var.bar=baz"
      (Left
        { scheme: Scheme.unsafeFromString "sql2"
        , hierPart: HierarchicalPartNoAuth (Just (Left (PathAbsolute Nothing)))
        , query: Just (Query.unsafeFromString "q=foo&var.bar=baz")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:?q=foo&var.bar=baz"
      (Left
        { scheme: Scheme.unsafeFromString "sql2"
        , hierPart: HierarchicalPartNoAuth Nothing
        , query: Just (Query.unsafeFromString "q=foo&var.bar=baz")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://localhost"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "https://1a.example.com"
      (Left
        { scheme: Scheme.unsafeFromString "https"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "1a.example.com"))))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://en.wikipedia.org/wiki/URI_scheme"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "en.wikipedia.org"))))
            }
          , path: path ["wiki", "URI_scheme"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://192.168.0.1"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
              { userInfo: Nothing
              , hosts: Just (This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1)))
              }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "sysop:moon")))
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
              { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "sysop:moon")))
              , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
              }
          , path: path [""]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/records"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
              { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "sysop:moon")))
              , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
              }
          , path: path ["records"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/records/etc/"
      (Left
        { scheme: Scheme.unsafeFromString "mongodb"
        , hierPart: HierarchicalPartAuth
          { authority:
              { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "sysop:moon")))
              , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))))
              }
          , path: path ["records", "etc", ""]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (IPv6Address (IPv6Address.unsafeFromString "2001:cdba:0000:0000:0000:0000:3257:9652")))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[FE80::0202:B3FF:FE1E:8329]"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (IPv6Address (IPv6Address.unsafeFromString "FE80::0202:B3FF:FE1E:8329")))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[2001:db8::1]:80"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (IPv6Address (IPv6Address.unsafeFromString "2001:db8::1")) (Port.unsafeFromInt 80))
            }
          , path: path []
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      (Left
        { scheme: Scheme.unsafeFromString "ftp"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "ftp.is.co.za"))))
            }
          , path: path ["rfc", "rfc1808.txt"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://www.ietf.org/rfc/rfc2396.txt"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "www.ietf.org"))))
            }
          , path: path ["rfc", "rfc2396.txt"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      (Left
        { scheme: Scheme.unsafeFromString "ldap"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (IPv6Address (IPv6Address.unsafeFromString "2001:db8::7")))
            }
          , path: path ["c=GB"]
          }
        , query: (Just (Query.unsafeFromString "objectClass?one"))
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "telnet://192.0.2.16:80/"
      (Left
        { scheme: Scheme.unsafeFromString "telnet"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (IPv4Address (IPv4Address.unsafeFromInts 192 0 2 16)) (Port.unsafeFromInt 80))
            }
          , path: path [""]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://example.com:8042/over/there?name=ferret#nose"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "example.com"))) (Port.unsafeFromInt 8042))
            }
          , path: path ["over", "there"]
          }
        , query: (Just (Query.unsafeFromString "name=ferret"))
        , fragment: (Just (Fragment.unsafeFromString "nose"))
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://example.com:8042/over/there?name=ferret#"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "example.com"))) (Port.unsafeFromInt 8042))
            }
          , path: path ["over", "there"]
          }
        , query: (Just (Query.unsafeFromString "name=ferret"))
        , fragment: (Just (Fragment.unsafeFromString ""))
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://info.example.com?fred"
      (Left
        { scheme: Scheme.unsafeFromString "foo"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "info.example.com"))))
            }
          , path: path []
          }
        , query: (Just (Query.unsafeFromString "fred"))
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      (Left
        { scheme: Scheme.unsafeFromString "ftp"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: (Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "cnn.example.com&story=breaking_news"))))
            , hosts: Just (This (IPv4Address (IPv4Address.unsafeFromInts 10 0 0 1)))
            }
          , path: path ["top_story.htm"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "top_story.htm"
      (Right
        { relPart: RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString $ nes (SProxy :: SProxy "top_story.htm")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "../top_story.htm"
      (Right
        { relPart: RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString $ nes (SProxy :: SProxy "..")) [PathSegment.unsafeSegmentFromString "top_story.htm"]))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "/top_story.htm"
      (Right
        { relPart: RelativePartNoAuth (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "top_story.htm")) [])))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "/"
      (Right
        { relPart: RelativePartNoAuth (Just (Left (PathAbsolute Nothing)))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      ""
      (Right
        { relPart: RelativePartNoAuth Nothing
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://www.example.com/some%20invented/url%20with%20spaces.html"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "www.example.com"))))
            }
          , path: path ["some%20invented", "url%20with%20spaces.html"]
          }
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (Both (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "localhost"))) (Port.unsafeFromInt 53174))
            }
          , path: path ["metadata", "fs", "test", "%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23"]
          }
        , query: Just (Query.unsafeFromString "")
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "news:comp.infosystems.www.servers.unix"
      (Left
        { scheme: Scheme.unsafeFromString "news"
        , hierPart: HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "comp.infosystems.www.servers.unix")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "tel:+1-816-555-1212"
      (Left
        { scheme: Scheme.unsafeFromString "tel"
        , hierPart: HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "+1-816-555-1212")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
      (Left
        { scheme: Scheme.unsafeFromString "urn"
        , hierPart: HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "oasis:names:specification:docbook:dtd:xml:4.1.2")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mailto:John.Doe@example.com"
      (Left
        { scheme: Scheme.unsafeFromString "mailto"
        , hierPart: HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "John.Doe@example.com")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mailto:fred@example.com"
      (Left
        { scheme: Scheme.unsafeFromString "mailto"
        , hierPart: HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (SProxy :: SProxy "fred@example.com")) []))))
        , query: Nothing
        , fragment: Nothing
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "local.slamdata.com"))))
            }
          , path: path [""]
          }
        , query: Just (Query.unsafeFromString "")
        , fragment: Just (Fragment.unsafeFromString "?sort=asc&q=path%3A%2F&salt=1177214")
        })
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://local.slamdata.com/?#?sort=asc&q=path:/&salt=1177214"
      (Left
        { scheme: Scheme.unsafeFromString "http"
        , hierPart: HierarchicalPartAuth
          { authority:
            { userInfo: Nothing
            , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "local.slamdata.com"))))
            }
          , path: path [""]
          }
        , query: Just (Query.unsafeFromString "")
        , fragment: Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214")
        })

path ∷ Array String → Path
path = Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (URIRefOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
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
