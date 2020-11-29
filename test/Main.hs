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

import Control.Applicative
import Data.List (sort)
import Data.SemVer
import Data.SemVer.Constraint
import Data.Text (Text, unpack)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "serialisation"
          [ testGroup
              "isomorphisms"
              [ iso "0.0.0",
                iso "1.0.0",
                iso "1.0.0-alpha",
                iso "1.0.0-alpha.1",
                iso "1.0.1",
                iso "1.1.0",
                iso "2.0.0",
                iso "2.1.0",
                iso "2.1.1",
                iso "1.2.3+sha.2ac",
                iso "1.2.3-beta.1+sha.exp.dc2"
              ]
          ],
        testGroup
          "precedence"
          [ testCase "0.0.0 < 1.0.0 < 1.0.1 < 1.1.0" $
              [sv000, sv100, sv101, sv110] @=? sort [sv101, sv110, sv100, sv000],
            testCase "1.0.0 < 2.0.0" $
              true (sv100 < sv200),
            testCase "1.0.0-alpha < 1.0.0" $
              true (sv100alpha < sv100),
            testCase "1.0.0-alpha < 1.0.0-alpha.1" $
              true (sv100alpha < sv100alpha1),
            testCase "1.2.3-beta.1+sha.exp.dc2 < 1.2.3+sha.2ac" $
              true (sv123beta1shaexpdc2 < sv123sha2ac)
          ],
        testGroup
          "equality"
          [ testCase "0.0.0 == 0.0.0" $
              sv000 @=? sv000,
            testCase "compare 0.0.0 0.0.0 == EQ" $
              (sv000 `compare` sv000) @=? EQ,
            testCase "1.2.3 /= 1.2.3+sha.2ac" $
              (sv123 /= sv123sha2ac) @=? True,
            testCase "compare 1.2.3 1.2.3+sha.2ac == EQ" $
              (sv123 `compare` sv123sha2ac) @=? EQ
          ],
        testGroup
          "constraint satisfaction"
          [ testGroup
              "basic satisfaction"
              [ testCase "1.2.3 `satisfies` >=1.2.3" $ true (sv123 `satisfies` sc ">=1.2.3"),
                testCase "not $ 1.2.3 `satisfies` >1.2.3" $ false (sv123 `satisfies` sc ">1.2.3"),
                testCase "1.2.3 `satisfies` <1.2.4" $ true (sv123 `satisfies` sc "<1.2.4"),
                testCase "not $ 1.2.3 `satisfies` =1.2.4" $ false (sv123 `satisfies` sc "=1.2.4")
              ],
            testGroup
              "composite satisfaction"
              [ testCase "1.2.3 `satisfies` >1.0.0 <2.0.0" $ true (sv123 `satisfies` sc ">1.0.0 <2.0.0"),
                testCase "1.2.3 `satisfies` <2.0.0 || >3.0.0" $ true (sv123 `satisfies` sc "<2.0.0 || >3.0.0"),
                testCase "1.2.3 `satisfies` >1.0.0 <2.0.0 || 3.0.0" $ true (sv123 `satisfies` sc ">1.0.0 <2.0.0 || 3.0.0"),
                testCase "1.2.3 `satisfies` 3.0.0 || <2.0.0 >1.0.0" $ true (sv123 `satisfies` sc "3.0.0 || <2.0.0 >1.0.0")
              ],
            testGroup
              "prerelease handling"
              [ testCase "prerelease versions satisfy if triple is same" $ true (sv100alpha `satisfies` sc ">=1.0.0-alpha"),
                testCase "prerelease versions don't satisfy if spec doesn't contain prerelease tag on same triple" $ false (sv100alpha `satisfies` sc ">=1.0.0"),
                testCase "prerelease versions don't satisfy if spec doesn't contain prerelease tag on same triple" $ false (sv "3.0.0-alpha" `satisfies` sc ">=1.0.0-alpha"),
                testCase "prerelease versions do not require tag to be the same to satisfy" $ true (sv "1.0.0-beta" `satisfies` sc ">=1.0.0-alpha"),
                testCase "regular versions can satisfy prerelease constraints" $ true (sv "3.0.0" `satisfies` sc ">=2.0.0-alpha")
              ]
          ]
      ]

iso :: Text -> TestTree
iso t = testCase (unpack t) (Right t @=? (toText <$> Data.SemVer.fromText t))

true :: Bool -> Assertion
true = (True @=?)

false :: Bool -> Assertion
false = (False @=?)

sv000, sv100, sv100alpha, sv100alpha1, sv101 :: Version
sv110, sv200, sv123sha2ac, sv123beta1shaexpdc2 :: Version
sv000 = initial
sv100 = sv "1.0.0"
sv100alpha = sv "1.0.0-alpha"
sv100alpha1 = sv "1.0.0-alpha.1"
sv101 = sv "1.0.1"

sv110 = sv "1.1.0"

sv200 = sv "2.0.0"

sv123 = sv "1.2.3"

sv123sha2ac = sv "1.2.3+sha.2ac"

sv123beta1shaexpdc2 = sv "1.2.3-beta.1+sha.exp.dc2"

sv :: Text -> Version
sv t = case Data.SemVer.fromText t of
  Left e -> error e
  Right x -> x

sc :: Text -> Constraint
sc t = case Data.SemVer.Constraint.fromText t of
  Left e -> error e
  Right x -> x
