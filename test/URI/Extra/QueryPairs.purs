module Test.URI.Extra.QueryPairs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Common (wrapParser)
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

spec ∷ Spec Unit
spec =
  describe "QueryPairs printer/parser" do
    let parser = wrapParser (NQP.parse pure pure) Query.parser
    let printer = Query.print <<< NQP.print identity identity
    testIso parser printer
      "?key1=value1&key2=value2&key1=value3"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "key1") (Just (NQP.unsafeValueFromString "value1"))
        , Tuple (NQP.unsafeKeyFromString "key2") (Just (NQP.unsafeValueFromString "value2"))
        , Tuple (NQP.unsafeKeyFromString "key1") (Just (NQP.unsafeValueFromString "value3"))
        ])
    testIso parser printer
      "?state=source-add%3a636bb876-a59b-4516-8a4e-31a90d6201a9&code=eyJraWRiOiJjcGlTY29yZV8wOTI1MjAxNSNsInZlciI6IjEuMCIsInppcCI6IkRlZmxhdGUiLCJzZXIiOiIxLjAifQ..POoi6BeVmzh85nqw.2DBovxH_VhgEn0KQ67VlAOgPmJYxqJN-R4mrN45jGRL6Z4MiQQxn1xHmrgnvkWZ4zbTaUE8BtPoJw7nke6pLtiQqwo9y7Sf4MkkSuVbEg2b1lfmZ1gOCfRLFzAW5mWQrPEs5Vie4cjnhOZi-EkgQUQVP73apF8yjhKIZlrLdrbJV9ta8yf394rXKHeLUBU19F4tpf7zSKSJBBRfQDyouL4FbReRmnR9UHR6sixkOteNChJDoV0mnrPHPgaxaNpGLx4a1tR9DRdQnaqYUTMR0cbTEoJ1IfObKfPGdftJ2G_rsqfs9xGbmsow5BhAmF4CC9vzHtNvgPt_-N7Tz3mkTxhmX67tNT3YAgsRamduk9j_5h6aiTckh7XwcBhDbgp0PoyEXZ9mXatghLtJRZ-7B72WPKYTZs2jkF1Ir1E950I6sc2m3uw09zaGYFmxjSba-BipPD_lMzyVfIYThF_kHDHNspYb77ilhMuOhVsBkJ_-sQxPf5KdjzSGkLkgPBaZ8cCdgnantGc_DJWfl-NJWKZLNZ6w3j6nCWW4c19wjXrG-h2C8rseudSoLy_IGQSsV-4uQLKwBSRMvfmk7ysLlfBFfHD4z3XGbuyJ2vsEzVOJngl-Ynyiac8nHt8yr7pi3Bvsi9CfsYKzMbxXBreQ3EM0pzOIdnPp5IgdfOMYU0wShE4OVspmP2ppiBcWZdrD55gYNgaHjD0jJWSJR80uPEx7nt2KW3JToXBcb0MxUcDa_2A0EbsOU578NZct4MEJsVDBdfGRC6XB55g7ZNwrYCmlzMeWt2nI_QIZArNM-5dfqjlvL7uvsKQg37gJDpCdNPWV1DVC3FAU1AVwZLeyYLk2CwN4OlGxXWLfxdo6-hgAg6_mS9HXZj05Krb4geV5MiSNCHIx3IPWe_LcXlRaZCjBO8YVipIZkpyh-JQywFl2lexb-t1M1450JozXry6Afd5ek4OrZpAlFI-KDP7y1RbgBoqYloySGGvTYItclFw7gsST42B1D1OWDDWacS9p6C-xl6wDONzRXU3xOBhifLJL6JGboZIadcupYl5AKh18MivIrt5oSd2BaLuMzax9YqSYJ2U8WpYp3QRkLbokhLq_D_whV-0R6zjDHk8t3zGXiRj9JXazTcirf8emYuigMaVOW9m6KcUeHhUM_gn4oATmZ3WfvnVqSSz72j8p_GwRM3P9C_nn1jOAuxA5j66KTpQTgLj7HIkTP-sm30qIe5PwWaWQrbPenI_Zfu03JMqTXiQOCVxSwmkjYbQkWYq7p1-0Ct4Wqwt7cyfMYnDhD8JZauIO2XA5eLuTRJm3ZEYclq0IStGx_HwJSERJtuHHOGQlnEYGxKAyYb8X9jwlHe4WYwQN9Sg7uIYVDUZ5R5k_Ol5KP2jyQLO0m9aRu2-pF05w8GDU52FJnjh2LvvQXkPH2pgY2rx65sss4hqj7B0h--WdFCGpT3cLSKJndJ6XfrF8uKGf_6WvQdcxggBkckeO1pkU9etll-Aq_Dxh6St0PCgegH9Do2QXfY6X0iEnZ5-r4BGmV3HjlFwtcAR5PDv8F71wmuz2GQbZnnQBrgtBIIkILHAAnBiVy-8Tl2QmKlkieEhISrju1fMSNnc5ZQ_vggqpeN47wTzsklY0z6liAfVMsvpoCAJm-g4FroYbVSha33Lc6lUv6jxIhdveidFzKDi8Sh_i8H3XwXI68FzgmSQDzOrZkAcPkoJItq_EROxy4KC-i7KZviZMVefQ0Fktd41fM0ik5LwWl5PgGKoFy_Cy8VsMMMsOurYGBj8YdjDxzz6wqOVbNPcPuLqJHJY9StEtqqhwVCwdfGqVSD_VPn6B_3RTXKQSGt5fCXIJNvY8170HTfyioE-ixPfBErTIny5FLiSokqmrTvCH3l1XfCd2Ee8zoLaUqnzOOOE4467vFVuxZ4CfIQLt6jIxJ0Tb0BOTIBkOELFjEZ0owEiX63eaxRF7f2sst78HN1V1NzEf1WNdbikJmE1TPX1KXNIaF2nEVpfVxVN3aWUu_nNzOUVLEF55dnrTJdgy8wsF7wRtflW1GfFOfA_D2im2dDCg688R3LcVjddpg-VYwuZ7FH00xXWkoy85h9aCX8OnmfahNpuwyBcXkYDx8X4pATt-ZW6XOK7eajZnKJedMrlVFv2Ll7dwnyv74p8fYXF85ilTgRJeE5j7Tkss1gt5zHjihLJq_256DhMKMTFyTa6D_Hp0MH5HqP6SoTE8qzFSvHz_skRtVgmOrtAJ4-ZGfhXuDFhqMTehmmIF9oCirWs7qZHq2stgYoNgmIAyqgCXkvt39YEkaUhnBdt71URQ9XjCPToLSnUmcaFhV2kzoj7PkunTa4saS0ARvDXP_z3mEZ-e9II-ASwfScJV6OqyDABeuKGkxOB_ddUxnXyL1F2K0X1qzi1kJ-TVmLDseN805hl3gqPOvbdh65mAPrDw4713a3LRsOiRfDjDmR3GE3QxyTYrXFGMiGShhuCjZBZKkqw4OJqX9alY9HrwvIk7wcBlXYUcjU5G1qUK0jP8ozRRAiyO8QRVkkI830NAF52RuhxKshkx7gGQaNLU-pQGv8aPCXi2rosYJfhqlqEQ16yhezRAh2591jCLNCcDP-XXIUyrrpWZO3nHvTUfsovFJlGYrwugPulF7PSoNBBuX7rYbLaORdGB8Hi3iFnRo_tJ-kBcH03aNOLWaRO0bLFmJveJjtPsTmIbSr6wKiYxfmROMjrHDI-_ATj7x6pDJUU2IAqauZgAUYZ_ddK1z7N76CkRtXnAj1LmsEULoyVqYjTo6ggKWBnamUEltVWPHuY3IuLsmpda2kdd7--KektSvGct0aJLc84gqQdJeQCeoIQ0pSeYHMwEU61AdZk6a2xQtKZwUOvLbp5DGXgHCqx9H3H0Qj_u-iVcEEgozY3NerPkTr_AsAUGVE1vT4HrUje0sxdE_X4d5CKXafuKd4POHTVs9Y4T7icdMn4rShSnyc4NiFzUyw89-rcf014jm1ll7bQSuRMsvs9x96hvXhC1syoBR3wSt9cRnHQuEPBr48eNwtd7vgmXVPFFRa5vF_Hl8pU0e8tvuCwB2HFO6dMoHAKKlp199goNTv7Y2xF0c1jx0QbuZ0MlcVatwZsBpqbgd6_WJZn4768uKAALHJOIDkNvquq3h8nOzlpmtjNn5cGcfzHuaqwj3qiuVboA14WoJ-FT4vr_enTHO_zoeLZVUmVLR9t36Yc5dg7teuAAn9lfUukHJ1mTbMZaFBtQj6kUHWJKc_T5vHdoVFTTdTex--RQUxc97NnknlPqKOuKOO2ECyYuW8ygt4IcP4iSMWQmdXS74TEdg8I9Qtc42rzYgwb16phKPv1GpXYdCHDl-SXe9EnY2FQJiWBFYTFIOwl4PKmygbaHO2qgYtx4MUsLFiEvfgabxGqP_UBUUlPvp3iWWYvbsNqHD0sS-M9gdwGtS7ejPs9ika2NHKiOjJrJKEPjpkHuY1pdmAu5WhzH2GvXECTKrJRJhxIQoBhLxrBUVDr9iXBdDUoT3NIVuqg56HDjVinD4KG8aba5klnPibYcDtUXgssoE1rEKcfCEN8gR7-Xf20y1hQ8vhC34ayVAUVTVW3V5LoOcdSqcRJR71PiVKjTVRDrP9KdQCUWzsb0toQ950x14yGzk6cI0ZRgiKx-sRjTWlb5c2QzuD22vLGF9mWjGH-4NPhlrasr5PcdXeNmpBEZU_xKEujeuveE7GIzbMfZ-_E45Rlqflcn5ZdIK0-0HMjrZ_sjnuQ.ZjIHxProSKoNwD6Py-8FOQ"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "state") (Just (NQP.unsafeValueFromString "source-add%3a636bb876-a59b-4516-8a4e-31a90d6201a9"))
        , Tuple (NQP.unsafeKeyFromString "code") (Just (NQP.unsafeValueFromString "eyJraWQiOiJjcGltY29yZV8wOTI1MjAxNSIsInZlciI6IjEuMCIsInppcCI6IkRlZmxhdGUiLCJzZXIiOiIxLjAifQ..POoi6BeVmzh85nqw.2DBovxH_VhgEn0KQ67VlAOgPmJYxqJN-R4mrN45jGRL6Z4MiQQxn1xHmrgnvkWZ4zbTaUE8BtPoJw7nke6pLtiQqwo9y7Sf4MkkSuVbEg2b1lfmZ1gOCfRLFzAW5mWQrPEs5Vie4cjnhOZi-EkgQUQVP73apF8yjhKIZlrLdrbJV9ta8yf394rXKHeLUBU19F4tpf7zSKSJBBRfQDyouL4FbReRmnR9UHR6sixkOteNChJDoV0mnrPHPgaxaNpGLx4a1tR9DRdQnaqYUTMR0cbTEoJ1IfObKfPGdftJ2G_rsqfs9xGbmsow5BhAmF4CC9vzHtNvgPt_-N7Tz3mkTxhmX67tNT3YAgsRamduk9j_5h6aiTckh7XwcBhDbgp0PoyEXZ9mXatghLtJRZ-7B72WPKYTZs2jkF1Ir1E950I6sc2m3uw09zaGYFmxjSba-BipPD_lMzyVfIYThF_kHDHNspYb77ilhMuOhVsBkJ_-sQxPf5KdjzSGkLkgPBaZ8cCdgnantGc_DJWfl-NJWKZLNZ6w3j6nCWW4c19wjXrG-h2C8rseudSoLy_IGQSsV-4uQLKwBSRMvfmk7ysLlfBFfHD4z3XGbuyJ2vsEzVOJngl-Ynyiac8nHt8yr7pi3Bvsi9CfsYKzMbxXBreQ3EM0pzOIdnPp5IgdfOMYU0wShE4OVspmP2ppiBcWZdrD55gYNgaHjD0jJWSJR80uPEx7nt2KW3JToXBcb0MxUcDa_2A0EbsOU578NZct4MEJsVDBdfGRC6XB55g7ZNwrYCmlzMeWt2nI_QIZArNM-5dfqjlvL7uvsKQg37gJDpCdNPWV1DVC3FAU1AVwZLeyYLk2CwN4OlGxXWLfxdo6-hgAg6_mS9HXZj05Krb4geV5MiSNCHIx3IPWe_LcXlRaZCjBO8YVipIZkpyh-JQywFl2lexb-t1M1450JozXry6Afd5ek4OrZpAlFI-KDP7y1RbgBoqYloySGGvTYItclFw7gsST42B1D1OWDDWacS9p6C-xl6wDONzRXU3xOBhifLJL6JGboZIadcupYl5AKh18MivIrt5oSd2BaLuMzax9YqSYJ2U8WpYp3QRkLbokhLq_D_whV-0R6zjDHk8t3zGXiRj9JXazTcirf8emYuigMaVOW9m6KcUeHhUM_gn4oATmZ3WfvnVqSSz72j8p_GwRM3P9C_nn1jOAuxA5j66KTpQTgLj7HIkTP-sm30qIe5PwWaWQrbPenI_Zfu03JMqTXiQOCVxSwmkjYbQkWYq7p1-0Ct4Wqwt7cyfMYnDhD8JZauIO2XA5eLuTRJm3ZEYclq0IStGx_HwJSERJtuHHOGQlnEYGxKAyYb8X9jwlHe4WYwQN9Sg7uIYVDUZ5R5k_Ol5KP2jyQLO0m9aRu2-pF05w8GDU52FJnjh2LvvQXkPH2pgY2rx65sss4hqj7B0h--WdFCGpT3cLSKJndJ6XfrF8uKGf_6WvQdcxggBkckeO1pkU9etll-Aq_Dxh6St0PCgegH9Do2QXfY6X0iEnZ5-r4BGmV3HjlFwtcAR5PDv8F71wmuz2GQbZnnQBrgtBIIkILHAAnBiVy-8Tl2QmKlkieEhISrju1fMSNnc5ZQ_vggqpeN47wTzsklY0z6liAfVMsvpoCAJm-g4FroYbVSha33Lc6lUv6jxIhdveidFzKDi8Sh_i8H3XwXI68FzgmSQDzOrZkAcPkoJItq_EROxy4KC-i7KZviZMVefQ0Fktd41fM0ik5LwWl5PgGKoFy_Cy8VsMMMsOurYGBj8YdjDxzz6wqOVbNPcPuLqJHJY9StEtqqhwVCwdfGqVSD_VPn6B_3RTXKQSGt5fCXIJNvY8170HTfyioE-ixPfBErTIny5FLiSokqmrTvCH3l1XfCd2Ee8zoLaUqnzOOOE4467vFVuxZ4CfIQLt6jIxJ0Tb0BOTIBkOELFjEZ0owEiX63eaxRF7f2sst78HN1V1NzEf1WNdbikJmE1TPX1KXNIaF2nEVpfVxVN3aWUu_nNzOUVLEF55dnrTJdgy8wsF7wRtflW1GfFOfA_D2im2dDCg688R3LcVjddpg-VYwuZ7FH00xXWkoy85h9aCX8OnmfahNpuwyBcXkYDx8X4pATt-ZW6XOK7eajZnKJedMrlVFv2Ll7dwnyv74p8fYXF85ilTgRJeE5j7Tkss1gt5zHjihLJq_256DhMKMTFyTa6D_Hp0MH5HqP6SoTE8qzFSvHz_skRtVgmOrtAJ4-ZGfhXuDFhqMTehmmIF9oCirWs7qZHq2stgYoNgmIAyqgCXkvt39YEkaUhnBdt71URQ9XjCPToLSnUmcaFhV2kzoj7PkunTa4saS0ARvDXP_z3mEZ-e9II-ASwfScJV6OqyDABeuKGkxOB_ddUxnXyL1F2K0X1qzi1kJ-TVmLDseN805hl3gqPOvbdh65mAPrDw4713a3LRsOiRfDjDmR3GE3QxyTYrXFGMiGShhuCjZBZKkqw4OJqX9alY9HrwvIk7wcBlXYUcjU5G1qUK0jP8ozRRAiyO8QRVkkI830NAF52RuhxKshkx7gGQaNLU-pQGv8aPCXi2rosYJfhqlqEQ16yhezRAh2591jCLNCcDP-XXIUyrrpWZO3nHvTUfsovFJlGYrwugPulF7PSoNBBuX7rYbLaORdGB8Hi3iFnRo_tJ-kBcH03aNOLWaRO0bLFmJveJjtPsTmIbSr6wKiYxfmROMjrHDI-_ATj7x6pDJUU2IAqauZgAUYZ_ddK1z7N76CkRtXnAj1LmsEULoyVqYjTo6ggKWBnamUEltVWPHuY3IuLsmpda2kdd7--KektSvGct0aJLc84gqQdJeQCeoIQ0pSeYHMwEU61AdZk6a2xQtKZwUOvLbp5DGXgHCqx9H3H0Qj_u-iVcEEgozY3NerPkTr_AsAUGVE1vT4HrUje0sxdE_X4d5CKXafuKd4POHTVs9Y4T7icdMn4rShSnyc4NiFzUyw89-rcf014jm1ll7bQSuRMsvs9x96hvXhC1syoBR3wSt9cRnHQuEPBr48eNwtd7vgmXVPFFRa5vF_Hl8pU0e8tvuCwB2HFO6dMoHAKKlp199goNTv7Y2xF0c1jx0QbuZ0MlcVatwZsBpqbgd6_WJZn4768uKAALHJOIDkNvquq3h8nOzlpmtjNn5cGcfzHuaqwj3qiuVboA14WoJ-FT4vr_enTHO_zoeLZVUmVLR9t36Yc5dg7teuAAn9lfUukHJ1mTbMZaFBtQj6kUHWJKc_T5vHdoVFTTdTex--RQUxc97NnknlPqKOuKOO2ECyYuW8ygt4IcP4iSMWQmdXS74TEdg8I9Qtc42rzYgwb16phKPv1GpXYdCHDl-SXe9EnY2FQJiWBFYTFIOwl4PKmygbaHO2qgYtx4MUsLFiEvfgabxGqP_UBUUlPvp3iWWYvbsNqHD0sS-M9gdwGtS7ejPs9ika2NHKiOjJrJKEPjpkHuY1pdmAu5WhzH2GvXECTKrJRJhxIQoBhLxrBUVDr9iXBdDUoT3NIVuqg56HDjVinD4KG8aba5klnPibYcDtUXgssoE1rEKcfCEN8gR7-Xf20y1hQ8vhC34ayVAUVTVW3V5LoOcdSqcRJR71PiVKjTVRDrP9KdQCUWzsb0toQ950x14yGzk6cI0ZRgiKx-sRjTWlb5c2QzuD22vLGF9mWjGH-4NPhlrasr5PcdXeNmpBEZU_xKEujeuveE7GIzbMfZ-_E45Rlqflcn5ZdIK0-0HMjrZ_sjnuQ.ZjIHxProSKoNwD6Py-8FOQ"))
        ])
    testIso parser printer
      "?k%3Dey=value%3D1%3B2"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "k%3Dey") (Just (NQP.unsafeValueFromString "value%3D1%3B2")) ])
    testIso parser printer
      "?k%3Dey=value%3D1%3B2"
      (NQP.QueryPairs
        [ Tuple (NQP.keyFromString "k=ey") (Just (NQP.valueFromString "value=1;2")) ])
    testIso parser printer
      "?"
      (NQP.QueryPairs [])
    testIso parser printer
      "?key1=&key2="
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "key1") (Just (NQP.unsafeValueFromString ""))
        , Tuple (NQP.unsafeKeyFromString "key2") (Just (NQP.unsafeValueFromString ""))
        ])
    testIso parser printer
      "?key1&key2"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "key1") Nothing
        , Tuple (NQP.unsafeKeyFromString "key2") Nothing
        ])
    testIso parser printer
      "?key1=foo%3Bbar"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "key1") (Just (NQP.unsafeValueFromString "foo%3Bbar"))
        ])
    testIso parser printer
      "?replicaSet=test&connectTimeoutMS=300000"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "replicaSet") (Just (NQP.unsafeValueFromString "test"))
        , Tuple (NQP.unsafeKeyFromString "connectTimeoutMS") (Just (NQP.unsafeValueFromString "300000"))
        ])
    testIso parser printer
      "?fred"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "fred") Nothing ])
    testIso parser printer
      "?objectClass?one"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "objectClass?one") Nothing
        ])
    testIso parser printer
      "?password=&docTypeKey="
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "password") (Just (NQP.unsafeValueFromString ""))
        , Tuple (NQP.unsafeKeyFromString "docTypeKey") (Just (NQP.unsafeValueFromString ""))
        ])
    testIso parser printer
      "?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (NQP.QueryPairs
        [ Tuple (NQP.unsafeKeyFromString "password") (Just (NQP.unsafeValueFromString "pass"))
        , Tuple (NQP.unsafeKeyFromString "docTypeKey") (Just (NQP.unsafeValueFromString "type"))
        , Tuple (NQP.unsafeKeyFromString "queryTimeoutSeconds") (Just (NQP.unsafeValueFromString "20"))
        ])
