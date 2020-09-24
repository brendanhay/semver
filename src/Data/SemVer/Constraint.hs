{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- Module      : Data.SemVer.Constraint
-- Copyright   : (c) 2020 Brendan Hay <brendan.g.hay@gmail.com>, Keagan McClelland <keagan.mcclelland@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | An implementation of the Semantic Versioning Constraints.
-- In absence of a standard around constraints, the behavior of node-semver is closely followed.
-- The behavior is outlined here: https://github.com/npm/node-semver#ranges
module Data.SemVer.Constraint
    ( Constraint(..)
    , satisfies
    , fromText
    )
where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.SemVer.Internal
import qualified Data.SemVer.Delimited         as DL
import           Data.Text                      ( Text )

data Constraint
    = CAny
    | CLt !Version
    | CLtEq !Version
    | CGt !Version
    | CGtEq !Version
    | CEq !Version
    | CAnd !Constraint !Constraint
    | COr !Constraint !Constraint
    deriving (Eq, Show)

-- | Checks whether the 'Version' satisfies the 'Constraint'
--
-- Note: Semantics of this are strange in the presence of pre-release identifiers. Without a proper standard for how
-- constraint satisfaction should behave, this implementation attempts to follow the behavior of node-semver
-- which can be found here: https://github.com/npm/node-semver#prerelease-tags.
--
-- This choice was made because node-semver is the most widely deployed implementation of semantic versioning with
-- the best documentation around how to treat pre-release identifiers.
--
-- The summary is that you must opt into using pre-release identifiers by specifying them in the constraint
-- and they __must__ match the __exact__ version that attempts to use a pre-release identifier.
satisfies :: Version -> Constraint -> Bool
satisfies version constraint = if containsPrerelease version
    then if not . null . filter ((triple version ==) . triple . snd) $ (prereleaseComparators constraint)
        then go version constraint
        else if constraint == CAny then True else False
    else go version constraint
  where
    triple :: Version -> (Int, Int, Int)
    triple = liftA3 (,,) _versionMajor _versionMinor _versionPatch
    containsPrerelease :: Version -> Bool
    containsPrerelease v = not . null . _versionRelease $ v
    -- this helps us gather the comparators that actually consented to prerelease versions
    prereleaseComparators :: Constraint -> [(Version -> Constraint, Version)]
    prereleaseComparators = \case
        CAny     -> []
        CLt   v  -> if containsPrerelease v then [(CLt, v)] else []
        CLtEq v  -> if containsPrerelease v then [(CLtEq, v)] else []
        CGt   v  -> if containsPrerelease v then [(CGt, v)] else []
        CGtEq v  -> if containsPrerelease v then [(CGtEq, v)] else []
        CEq   v  -> if containsPrerelease v then [(CEq, v)] else []
        CAnd a b -> prereleaseComparators a <> prereleaseComparators b
        COr  a b -> prereleaseComparators a <> prereleaseComparators b
    -- naive satisfaction checking
    go :: Version -> Constraint -> Bool
    go v c = case c of
        CAny     -> True
        CLt   vc -> v < vc
        CLtEq vc -> v <= vc
        CGt   vc -> v > vc
        CGtEq vc -> v >= vc
        CEq   vc -> v == vc
        CAnd a b -> go v a && go v b
        COr  a b -> go v a || go v b

-- | Parsing function to create a 'Constraint' from 'Text' according to the rules specified
-- here: https://github.com/npm/node-semver#ranges
--
-- Advanced syntax is not yet supported.
fromText :: Text -> Either String Constraint
fromText = parseOnly parser

parser :: Parser Constraint
parser = parserD DL.semantic

parserD :: Delimiters -> Parser Constraint
parserD d@Delimiters {..} = choice . fmap (<* endOfInput) $ [primP, andP, orP]
  where
    primP = choice
        [ char '*' *> pure CAny
        , char '<' *> (CLt <$> DL.parser d False)
        , string "<=" *> (CLtEq <$> DL.parser d False)
        , char '>' *> (CGt <$> DL.parser d False)
        , string ">=" *> (CGtEq <$> DL.parser d False)
        , CEq <$> ((option '=' $ char '=') *> DL.parser d False)
        ]
    andP = liftA2 CAnd primP (skipSpace *> (andP <|> primP))
    orP = liftA2 COr (andP <|> primP) (skipSpace *> string "||" *> skipSpace *> (orP <|> andP <|> primP))
