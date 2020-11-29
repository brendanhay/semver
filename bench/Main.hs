{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2014-2019 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Criterion
import Criterion.Main
import Data.List (sort)
import Data.SemVer
import Data.Text (Text, pack)
import Data.Version (parseVersion, showVersion)
import qualified Data.Version as Ver
import Text.ParserCombinators.ReadP

main :: IO ()
main =
  defaultMain
    [ bgroup
        "decoding"
        [ bgroup
            "semver"
            [ bgroup
                "fromText . pack"
                [ bench "1.2.3" $
                    nf (sv . pack) "1.2.3",
                  bench "1.2.3-alpha" $
                    nf (sv . pack) "1.2.3-alpha",
                  bench "1.2.3-alpha.1" $
                    nf (sv . pack) "1.2.3-alpha.1",
                  bench "1.2.3+123" $
                    nf (sv . pack) "1.2.3+123",
                  bench "1.2.3+sha.2ac" $
                    nf (sv . pack) "1.2.3+sha.2ac",
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf (sv . pack) "1.2.3-beta.1+sha.exp.dc2"
                ]
            ],
          bgroup
            "version"
            [ bgroup
                "parseVersion"
                [ bench "1.2.3" $
                    nf dv "1.2.3",
                  bench "1.2.3-alpha" $
                    nf dv "1.2.3-alpha",
                  bench "1.2.3-alpha.1" $
                    nf dv "1.2.3-alpha.1",
                  bench "1.2.3+123" $
                    nf dv "1.2.3+123",
                  bench "1.2.3+sha.2ac" $
                    nf dv "1.2.3+sha.2ac",
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf dv "1.2.3-beta.1+sha.exp.dc2"
                ]
            ]
        ],
      bgroup
        "encoding"
        [ bgroup
            "semver"
            [ bgroup
                "toLazyText"
                [ bench "1.2.3" $
                    nf toLazyText sv123,
                  bench "1.2.3-alpha" $
                    nf toLazyText sv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf toLazyText sv123alpha1,
                  bench "1.2.3+123" $
                    nf toLazyText sv123123,
                  bench "1.2.3+sha.2ac" $
                    nf toLazyText sv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf toLazyText sv123beta1shaexpdc2
                ],
              bgroup
                "toString"
                [ bench "1.2.3" $
                    nf toString sv123,
                  bench "1.2.3-alpha" $
                    nf toString sv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf toString sv123alpha1,
                  bench "1.2.3+123" $
                    nf toString sv123123,
                  bench "1.2.3+sha.2ac" $
                    nf toString sv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf toString sv123beta1shaexpdc2
                ]
            ],
          bgroup
            "version"
            [ bgroup
                "showVersion"
                [ bench "1.2.3" $
                    nf showVersion dv123,
                  bench "1.2.3-alpha" $
                    nf showVersion dv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf showVersion dv123alpha1,
                  bench "1.2.3+123" $
                    nf showVersion dv123123,
                  bench "1.2.3+sha.2ac" $
                    nf showVersion dv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf showVersion dv123beta1shaexpdc2
                ]
            ]
        ],
      bgroup
        "sort"
        [ bench "semver" $
            nf
              sort
              [ sv123,
                sv123alpha,
                sv123alpha1,
                sv123123,
                sv123sha2ac,
                sv123beta1shaexpdc2
              ],
          bench "version" $
            nf
              sort
              [ dv123,
                dv123alpha,
                dv123alpha1,
                dv123123,
                dv123sha2ac,
                dv123beta1shaexpdc2
              ]
        ]
    ]

sv123, sv123alpha, sv123alpha1, sv123123 :: Version
sv123sha2ac, sv123beta1shaexpdc2 :: Version
sv123 = sv "1.2.3"
sv123alpha = sv "1.2.3-alpha"
sv123alpha1 = sv "1.2.3-alpha.1"
sv123123 = sv "1.2.3+123"

sv123sha2ac = sv "1.2.3+sha.2ac"

sv123beta1shaexpdc2 = sv "1.2.3-beta.1+sha.exp.dc2"

dv123, dv123alpha, dv123alpha1, dv123123 :: Ver.Version
dv123sha2ac, dv123beta1shaexpdc2 :: Ver.Version
dv123 = dv "1.2.3"
dv123alpha = dv "1.2.3-alpha"
dv123alpha1 = dv "1.2.3-alpha.1"
dv123123 = dv "1.2.3+123"

dv123sha2ac = dv "1.2.3+sha.2ac"

dv123beta1shaexpdc2 = dv "1.2.3-beta.1+sha.exp.dc2"

sv :: Text -> Version
sv t = case fromText t of
  Left e -> error e
  Right x -> x

dv :: String -> Ver.Version
dv = fst . last . readP_to_S parseVersion
