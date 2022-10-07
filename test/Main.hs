{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

import qualified Data.List as List
import Data.SemVer (Version)
import qualified Data.SemVer as SemVer
import qualified Data.SemVer.QQ as SemVer.QQ
import Data.SemVer.Constraint (Constraint, satisfies)
import qualified Data.SemVer.Constraint as SemVer.Constraint
import Data.Text (Text)
import qualified Data.Text as Text
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
              [sv000, sv100, sv101, sv110] @=? List.sort [sv101, sv110, sv100, sv000],
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
          "constraint parsing and printing"
          ( ( ( \c ->
                  testCase
                    ("round-trip parsing " <> Text.unpack c)
                    ( SemVer.Constraint.toText <$> SemVer.Constraint.fromText c
                        @?= Right c
                    )
              )
                <$> [ ">=1.2.3",
                      ">1.2.3",
                      "<1.2.4",
                      "=1.2.4",
                      ">1.0.0 <2.0.0",
                      "<2.0.0 || >3.0.0",
                      ">1.0.0 <2.0.0 || =3.0.0",
                      "=3.0.0 || <2.0.0 >1.0.0",
                      ">=1.0.0-alpha",
                      ">=1.0.0",
                      ">=1.0.0-alpha",
                      ">=1.0.0-alpha",
                      ">=2.0.0-alpha"
                    ]
            )
              <> [ testCase "always uses the = sign" $
                     ( SemVer.Constraint.toText <$> SemVer.Constraint.fromText "1.0.0"
                         @?= Right "=1.0.0"
                     )
                 ]
              <> [ -- these tests are taken from the node-semver mostly verbatim:
                   -- https://github.com/npm/node-semver/blob/e79ac3a450e8bb504e78b8159e3efc70895699b8/test/fixtures/range-parse.js
                   --
                   -- Some of the tests have been modified to have equivalent
                   -- right hand sides however, due to differences in how we
                   -- pretty print / to account for the fact that we don't
                   -- normalize constraints before printing.
                   testGroup "node-semver package parsing test cases" $
                     let desugarsTo c result =
                           testCase (Text.unpack c <> " => " <> Text.unpack result) $
                             SemVer.Constraint.toText <$> SemVer.Constraint.fromText c
                               @?= Right result
                         failsToParse c =
                           testCase (Text.unpack c <> " fails to parse") $
                             case SemVer.Constraint.toText <$> SemVer.Constraint.fromText c of
                               Left _ -> pure ()
                               Right r -> error $ "expected parse failure but got: " <> Text.unpack r
                      in [ "1.0.0 - 2.0.0" `desugarsTo` ">=1.0.0 <=2.0.0",
                           "1 - 2" `desugarsTo` ">=1.0.0 <3.0.0",
                           "1.0 - 2.0" `desugarsTo` ">=1.0.0 <2.1.0",
                           ">=*" `desugarsTo` ">=0.0.0",
                           "*" `desugarsTo` "*",
                           -- in node-semver this parses to *; we don't do this
                           -- due to weird edge-cases
                           failsToParse "",
                           ">=1.0.0" `desugarsTo` ">=1.0.0",
                           ">1.0.0" `desugarsTo` ">1.0.0",
                           "<=2.0.0" `desugarsTo` "<=2.0.0",
                           "1" `desugarsTo` ">=1.0.0 <2.0.0",
                           "<=2.0.0" `desugarsTo` "<=2.0.0",
                           "<=2.0.0" `desugarsTo` "<=2.0.0",
                           "<2.0.0" `desugarsTo` "<2.0.0",
                           "<2.0.0" `desugarsTo` "<2.0.0",
                           ">= 1.0.0" `desugarsTo` ">=1.0.0",
                           ">=  1.0.0" `desugarsTo` ">=1.0.0",
                           ">=   1.0.0" `desugarsTo` ">=1.0.0",
                           "> 1.0.0" `desugarsTo` ">1.0.0",
                           ">  1.0.0" `desugarsTo` ">1.0.0",
                           "<=   2.0.0" `desugarsTo` "<=2.0.0",
                           "<= 2.0.0" `desugarsTo` "<=2.0.0",
                           "<=  2.0.0" `desugarsTo` "<=2.0.0",
                           "<    2.0.0" `desugarsTo` "<2.0.0",
                           "<\t2.0.0" `desugarsTo` "<2.0.0",
                           ">=0.1.97" `desugarsTo` ">=0.1.97",
                           ">=0.1.97" `desugarsTo` ">=0.1.97",
                           "0.1.20 || 1.2.4" `desugarsTo` "=0.1.20 || =1.2.4",
                           ">=0.2.3 || <0.0.1" `desugarsTo` ">=0.2.3 || <0.0.1",
                           ">=0.2.3 || <0.0.1" `desugarsTo` ">=0.2.3 || <0.0.1",
                           ">=0.2.3 || <0.0.1" `desugarsTo` ">=0.2.3 || <0.0.1",
                           -- in node-semver this parses to *; we don't due this
                           -- due to weird edge cases
                           failsToParse "||",
                           "2.x.x" `desugarsTo` ">=2.0.0 <3.0.0",
                           "1.2.x" `desugarsTo` ">=1.2.0 <1.3.0",
                           "1.2.x || 2.x" `desugarsTo` ">=1.2.0 <1.3.0 || >=2.0.0 <3.0.0",
                           "x" `desugarsTo` "*",
                           "2.*.*" `desugarsTo` ">=2.0.0 <3.0.0",
                           "1.2.*" `desugarsTo` ">=1.2.0 <1.3.0",
                           "1.2.* || 2.*" `desugarsTo` ">=1.2.0 <1.3.0 || >=2.0.0 <3.0.0",
                           "*" `desugarsTo` "*",
                           "2" `desugarsTo` ">=2.0.0 <3.0.0",
                           "2.3" `desugarsTo` ">=2.3.0 <2.4.0",
                           "~2.4" `desugarsTo` ">=2.4.0 <2.5.0",
                           "~2.4" `desugarsTo` ">=2.4.0 <2.5.0",
                           -- node-semver allows this to mean ">=3.2.1 <3.3.0"
                           -- but we disallow it because the interpretation of
                           -- that kind of syntax isn't well defined
                           failsToParse "~>3.2.1",
                           "~1" `desugarsTo` ">=1.0.0 <2.0.0",
                           -- node-semver allows these to mean ">=1.0.0 <2.0.0"
                           -- but we disallow it because the interpretation of
                           -- that kind of syntax isn't well defined
                           failsToParse "~>1",
                           failsToParse "~> 1",
                           "~1.0" `desugarsTo` ">=1.0.0 <1.1.0",
                           "~ 1.0" `desugarsTo` ">=1.0.0 <1.1.0",
                           "^0" `desugarsTo` ">=0.0.0 <1.0.0",
                           "^ 1" `desugarsTo` ">=1.0.0 <2.0.0",
                           "^0.1" `desugarsTo` ">=0.1.0 <0.2.0",
                           "^1.0" `desugarsTo` ">=1.0.0 <2.0.0",
                           "^1.2" `desugarsTo` ">=1.2.0 <2.0.0",
                           "^0.0.1" `desugarsTo` ">=0.0.1 <0.0.2",
                           "^0.0.1-beta" `desugarsTo` ">=0.0.1-beta <0.0.2",
                           "^0.1.2" `desugarsTo` ">=0.1.2 <0.2.0",
                           "^1.2.3" `desugarsTo` ">=1.2.3 <2.0.0",
                           "^1.2.3-beta.4" `desugarsTo` ">=1.2.3-beta.4 <2.0.0",
                           "<1" `desugarsTo` "<1.0.0",
                           "< 1" `desugarsTo` "<1.0.0",
                           ">=1" `desugarsTo` ">=1.0.0",
                           ">= 1" `desugarsTo` ">=1.0.0",
                           "<1.2" `desugarsTo` "<1.2.0",
                           "< 1.2" `desugarsTo` "<1.2.0",
                           "1" `desugarsTo` ">=1.0.0 <2.0.0",
                           -- node-semver rejects this but we accepted it with
                           -- the previous parser, so we accept it in the new
                           -- parser too
                           ">01.02.03" `desugarsTo` ">1.2.3",
                           failsToParse "~1.2.3beta",
                           "^ 1.2 ^ 1" `desugarsTo` ">=1.2.0 <2.0.0 >=1.0.0 <2.0.0",
                           "1.2 - 3.4.5" `desugarsTo` ">=1.2.0 <=3.4.5",
                           "1.2.3 - 3.4" `desugarsTo` ">=1.2.3 <3.5.0",
                           "1.2 - 3.4" `desugarsTo` ">=1.2.0 <3.5.0",
                           ">1" `desugarsTo` ">=2.0.0",
                           ">1.2" `desugarsTo` ">=1.3.0",
                           ">X" `desugarsTo` "<0.0.0",
                           "<X" `desugarsTo` "<0.0.0",
                           "<x <* || >* 2.x" `desugarsTo` "<0.0.0 <0.0.0 || <0.0.0 >=2.0.0 <3.0.0",
                           ">x 2.x || * || <x" `desugarsTo` "<0.0.0 >=2.0.0 <3.0.0 || * || <0.0.0",
                           -- below here are some extra tests not included in the
                           -- original node-semver test suite
                           "1.1.1-alpha.1 - 1.1.2-alpha.2"
                             `desugarsTo` ">=1.1.1-alpha.1 <=1.1.2-alpha.2",
                           "<=1.1.1-alpha.1" `desugarsTo` "<=1.1.1-alpha.1"
                         ]
                 ]
          ),
        testGroup
          "constraint satisfaction"
          [ testGroup
              "basic satisfaction"
              [ testCase "1.2.3 `satisfies` >=1.2.3" $
                  true (sv123 `satisfies` sc ">=1.2.3"),
                testCase "not $ 1.2.3 `satisfies` >1.2.3" $
                  false (sv123 `satisfies` sc ">1.2.3"),
                testCase "1.2.3 `satisfies` <1.2.4" $
                  true (sv123 `satisfies` sc "<1.2.4"),
                testCase "not $ 1.2.3 `satisfies` =1.2.4" $
                  false (sv123 `satisfies` sc "=1.2.4")
              ],
            testGroup
              "composite satisfaction"
              [ testCase "1.2.3 `satisfies` >1.0.0 <2.0.0" $
                  true (sv123 `satisfies` sc ">1.0.0 <2.0.0"),
                testCase "1.2.3 `satisfies` <2.0.0 || >3.0.0" $
                  true (sv123 `satisfies` sc "<2.0.0 || >3.0.0"),
                testCase "1.2.3 `satisfies` >1.0.0 <2.0.0 || 3.0.0" $
                  true (sv123 `satisfies` sc ">1.0.0 <2.0.0 || 3.0.0"),
                testCase "1.2.3 `satisfies` 3.0.0 || <2.0.0 >1.0.0" $
                  true (sv123 `satisfies` sc "3.0.0 || <2.0.0 >1.0.0")
              ],
            testGroup
              "prerelease handling"
              [ testCase "prerelease versions satisfy if triple is same" $
                  true (sv100alpha `satisfies` sc ">=1.0.0-alpha"),
                testCase "prerelease versions don't satisfy if spec doesn't contain prerelease tag on same triple" $
                  false (sv100alpha `satisfies` sc ">=1.0.0"),
                testCase "prerelease versions don't satisfy if spec doesn't contain prerelease tag on same triple" $
                  false (sv "3.0.0-alpha" `satisfies` sc ">=1.0.0-alpha"),
                testCase "prerelease versions do not require tag to be the same to satisfy" $
                  true (sv "1.0.0-beta" `satisfies` sc ">=1.0.0-alpha"),
                testCase "regular versions can satisfy prerelease constraints" $
                  true (sv "3.0.0" `satisfies` sc ">=2.0.0-alpha")
              ]
          ],
        testGroup
          "quasi-quoter"
          [ testGroup
              "as expression"
              [ testCase "can define a valid version" $
                  [SemVer.QQ.version|1.0.0-alpha|] @?= sv100alpha
              , testCase "can surround version with spaces" $
                  [SemVer.QQ.version| 1.0.0 |] @?= sv100
              ]
          , testGroup
              "as pattern"
              [ testCase "can use at pattern position" $
                  let isInit v = case v of [SemVer.QQ.version| 0.0.0 |] -> True; _ -> False
                  in map isInit [sv000, sv100] @?= [True, False]
              ]
          ]
      ]

iso :: Text -> TestTree
iso t = testCase (Text.unpack t) (Right t @=? (SemVer.toText <$> SemVer.fromText t))

true :: Bool -> Assertion
true = (True @=?)

false :: Bool -> Assertion
false = (False @=?)

sv000, sv100, sv100alpha, sv100alpha1, sv101 :: Version
sv110, sv200, sv123, sv123sha2ac, sv123beta1shaexpdc2 :: Version
sv000 = SemVer.initial
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
sv t =
  case SemVer.fromText t of
    Left e -> error e
    Right x -> x

sc :: Text -> Constraint
sc t =
  case SemVer.Constraint.fromText t of
    Left e -> error e
    Right x -> x
