{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Applicative
import Data.List           (sort)
import Data.SemVer
import Data.Text           (Text, unpack)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testGroup "serialisation"
        [ testGroup "isomorphisms"
            [ iso "0.0.0"
            , iso "1.0.0"
            , iso "1.0.0-alpha"
            , iso "1.0.0-alpha.1"
            , iso "1.0.1"
            , iso "1.1.0"
            , iso "2.0.0"
            , iso "2.1.0"
            , iso "2.1.1"
            , iso "1.2.3+sha.2ac"
            , iso "1.2.3-beta.1+sha.exp.dc2"
            ]
        ]

    , testGroup "precedence"
        [ testCase "0.0.0 < 1.0.0 < 1.0.1 < 1.1.0" $
            [sv000, sv100, sv101, sv110] @=? sort [sv101, sv110, sv100, sv000]
        , testCase "1.0.0 < 2.0.0" $
            true (sv100 < sv200)
        , testCase "1.0.0 < 1.0.0-alpha" $
            true (sv100 < sv100alpha)
        , testCase "1.0.0-alpha < 1.0.0-alpha.1" $
            true (sv100alpha < sv100alpha1)
        , testCase "1.2.3+sha.2ac < 1.2.3-beta.1+sha.exp.dc2" $
            true (sv123sha2ac < sv123beta1shaexpdc2)
        ]
    ]

iso :: Text -> TestTree
iso t = testCase (unpack t) (Right t @=? (toText <$> fromText t))

true :: Bool -> Assertion
true = (True @=?)

sv000, sv100, sv100alpha, sv100alpha1, sv101   :: Version
sv110, sv200, sv123sha2ac, sv123beta1shaexpdc2 :: Version
sv000               = defaultVersion
sv100               = sv "1.0.0"
sv100alpha          = sv "1.0.0-alpha"
sv100alpha1         = sv "1.0.0-alpha.1"
sv101               = sv "1.0.1"
sv110               = sv "1.1.0"
sv200               = sv "2.0.0"
sv123sha2ac         = sv "1.2.3+sha.2ac"
sv123beta1shaexpdc2 = sv "1.2.3-beta.1+sha.exp.dc2"

sv :: Text -> Version
sv t = case fromText t of
    Left  e -> error e
    Right x -> x
