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
import Data.Int
import Data.List             (intercalate)
import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf
import Data.SemVer

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testGroup "encoding"   []
    , testGroup "parsing"    []
    , testGroup "precedence" []
    ]
