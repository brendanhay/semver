{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Main (main) where

import Criterion
import Criterion.Main (defaultMain)
import qualified Data.List as List
import qualified Data.SemVer as SemVer
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Text.ParserCombinators.ReadP as ReadP

main :: IO ()
main =
  defaultMain
    [ bgroup
        "decoding"
        [ bgroup
            "semver"
            [ bgroup
                "fromText . Text.pack"
                [ bench "1.2.3" $
                    nf (sv . Text.pack) "1.2.3",
                  bench "1.2.3-alpha" $
                    nf (sv . Text.pack) "1.2.3-alpha",
                  bench "1.2.3-alpha.1" $
                    nf (sv . Text.pack) "1.2.3-alpha.1",
                  bench "1.2.3+123" $
                    nf (sv . Text.pack) "1.2.3+123",
                  bench "1.2.3+sha.2ac" $
                    nf (sv . Text.pack) "1.2.3+sha.2ac",
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf (sv . Text.pack) "1.2.3-beta.1+sha.exp.dc2"
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
                "SemVer.toLazyText"
                [ bench "1.2.3" $
                    nf SemVer.toLazyText sv123,
                  bench "1.2.3-alpha" $
                    nf SemVer.toLazyText sv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf SemVer.toLazyText sv123alpha1,
                  bench "1.2.3+123" $
                    nf SemVer.toLazyText sv123123,
                  bench "1.2.3+sha.2ac" $
                    nf SemVer.toLazyText sv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf SemVer.toLazyText sv123beta1shaexpdc2
                ],
              bgroup
                "SemVer.toString"
                [ bench "1.2.3" $
                    nf SemVer.toString sv123,
                  bench "1.2.3-alpha" $
                    nf SemVer.toString sv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf SemVer.toString sv123alpha1,
                  bench "1.2.3+123" $
                    nf SemVer.toString sv123123,
                  bench "1.2.3+sha.2ac" $
                    nf SemVer.toString sv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf SemVer.toString sv123beta1shaexpdc2
                ]
            ],
          bgroup
            "version"
            [ bgroup
                "Version.showVersion"
                [ bench "1.2.3" $
                    nf Version.showVersion dv123,
                  bench "1.2.3-alpha" $
                    nf Version.showVersion dv123alpha,
                  bench "1.2.3-alpha.1" $
                    nf Version.showVersion dv123alpha1,
                  bench "1.2.3+123" $
                    nf Version.showVersion dv123123,
                  bench "1.2.3+sha.2ac" $
                    nf Version.showVersion dv123sha2ac,
                  bench "1.2.3-beta.1+sha.exp.dc2" $
                    nf Version.showVersion dv123beta1shaexpdc2
                ]
            ]
        ],
      bgroup
        "List.sort"
        [ bench "semver" $
            nf
              List.sort
              [ sv123,
                sv123alpha,
                sv123alpha1,
                sv123123,
                sv123sha2ac,
                sv123beta1shaexpdc2
              ],
          bench "version" $
            nf
              List.sort
              [ dv123,
                dv123alpha,
                dv123alpha1,
                dv123123,
                dv123sha2ac,
                dv123beta1shaexpdc2
              ]
        ]
    ]

sv123, sv123alpha, sv123alpha1, sv123123 :: SemVer.Version
sv123sha2ac, sv123beta1shaexpdc2 :: SemVer.Version
sv123 = sv "1.2.3"
sv123alpha = sv "1.2.3-alpha"
sv123alpha1 = sv "1.2.3-alpha.1"
sv123123 = sv "1.2.3+123"

sv123sha2ac = sv "1.2.3+sha.2ac"

sv123beta1shaexpdc2 = sv "1.2.3-beta.1+sha.exp.dc2"

dv123, dv123alpha, dv123alpha1, dv123123 :: Version.Version
dv123sha2ac, dv123beta1shaexpdc2 :: Version.Version
dv123 = dv "1.2.3"
dv123alpha = dv "1.2.3-alpha"
dv123alpha1 = dv "1.2.3-alpha.1"
dv123123 = dv "1.2.3+123"

dv123sha2ac = dv "1.2.3+sha.2ac"

dv123beta1shaexpdc2 = dv "1.2.3-beta.1+sha.exp.dc2"

sv :: Text -> SemVer.Version
sv t =
  case SemVer.fromText t of
    Left e -> error e
    Right x -> x

dv :: String -> Version.Version
dv = fst . last . ReadP.readP_to_S Version.parseVersion
