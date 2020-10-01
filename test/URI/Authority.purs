module Test.URI.Authority where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Authority (AuthorityOptions, Host(..), Port, UserInfo)
import URI.Authority as Authority
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.UserInfo as UserInfo

spec ∷ Spec Unit
spec =
  describe "Authority parser/printer" do
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost"
      { userInfo: Nothing
      , hosts: Just (This (NameAddress (RegName.unsafeFromString (nes (SProxy :: SProxy "localhost")))))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//localhost:3000"
      { userInfo: Nothing
      , hosts: Just (Both (NameAddress (RegName.unsafeFromString (nes (SProxy :: SProxy "localhost")))) (Port.unsafeFromInt 3000))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//user@localhost:3000"
      { userInfo: Just (UserInfo.unsafeFromString (nes (SProxy :: SProxy "user")))
      , hosts: Just (Both (NameAddress (RegName.unsafeFromString (nes (SProxy :: SProxy "localhost")))) (Port.unsafeFromInt 3000))
      }
    testIso
      (Authority.parser options)
      (Authority.print options)
      "//:8000"
      { userInfo: Nothing
      , hosts: Just (That (Port.unsafeFromInt 8000))
      }

options ∷ Record (AuthorityOptions UserInfo (HostPortPair Host Port))
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  }
