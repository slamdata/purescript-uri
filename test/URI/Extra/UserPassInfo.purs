module Test.URI.Extra.UserPassInfo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Authority (Host(..), Port)
import URI.Authority as Authority
import URI.Extra.UserPassInfo (UserPassInfo(..))
import URI.Extra.UserPassInfo as UserPassInfo
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URIRef (Fragment, HierPath, Path, Query, RelPath, URIRefOptions)

spec ∷ Spec Unit
spec = do
  describe "Authority+UserPassInfo parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "user"), password: Nothing })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:pass@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "user"), password: Just (nes (SProxy :: SProxy "pass")) })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:pa%3Ass@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "user"), password: Just (nes (SProxy :: SProxy "pa:ss")) })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//us%3Aer:pa%3Ass@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "us:er"), password: Just (nes (SProxy :: SProxy "pa:ss")) })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//us%3Aer:pa%3Ass@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "us:er"), password: Just (nes (SProxy :: SProxy "pa:ss")) })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user:p%40ss@host"
      { userInfo: Just (UserPassInfo { user: nes (SProxy :: SProxy "user"), password: Just (nes (SProxy :: SProxy "p@ss")) })
      , hosts: Just (This (NameAddress (RegName.unsafeFromString $ nes (SProxy :: SProxy "host"))))
      }

options ∷ Record (URIRefOptions UserPassInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: UserPassInfo.parse
  , printUserInfo: UserPassInfo.print
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
