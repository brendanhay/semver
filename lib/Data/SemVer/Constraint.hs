{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.SemVer.Constraint
-- Copyright   : (c) 2020 Brendan Hay <brendan.g.hay@gmail.com>, Keagan McClelland <keagan.mcclelland@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- An implementation of the Semantic Versioning Constraints.
--
-- In the absence of a standard or specification for versioning constraints, the
-- behavior of node-semver is closely followed. This behavior is outlined <https://github.com/npm/node-semver#ranges here>.
module Data.SemVer.Constraint
  ( Constraint (..),
    satisfies,
    toText,
    fromText,
    parser,
    parserWith,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.SemVer.Delimited as Delimited
import Data.SemVer.Internal
import qualified Data.SemVer as SemVer
import Data.Text (Text)

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
satisfies version constraint =
  if containsPrerelease version
    then
      if not . null . filter ((triple version ==) . triple . snd) $ (prereleaseComparators constraint)
        then go version constraint
        else if constraint == CAny then True else False
    else go version constraint
  where
    triple :: Version -> (Int, Int, Int)
    triple = (,,) <$> _versionMajor <*> _versionMinor <*> _versionPatch

    containsPrerelease :: Version -> Bool
    containsPrerelease = not . null . _versionRelease

    -- this helps us gather the comparators that actually consented to prerelease versions
    prereleaseComparators :: Constraint -> [(Version -> Constraint, Version)]
    prereleaseComparators = \case
      CAny -> []
      CLt v -> if containsPrerelease v then [(CLt, v)] else []
      CLtEq v -> if containsPrerelease v then [(CLtEq, v)] else []
      CGt v -> if containsPrerelease v then [(CGt, v)] else []
      CGtEq v -> if containsPrerelease v then [(CGtEq, v)] else []
      CEq v -> if containsPrerelease v then [(CEq, v)] else []
      CAnd a b -> prereleaseComparators a <> prereleaseComparators b
      COr a b -> prereleaseComparators a <> prereleaseComparators b

    -- naive satisfaction checking
    go :: Version -> Constraint -> Bool
    go v c = case c of
      CAny -> True
      CLt vc -> v < vc
      CLtEq vc -> v <= vc
      CGt vc -> v > vc
      CGtEq vc -> v >= vc
      CEq vc -> v == vc
      CAnd a b -> go v a && go v b
      COr a b -> go v a || go v b

-- | Parsing function to create a 'Constraint' from 'Text' according to the rules specified
-- here: https://github.com/npm/node-semver#ranges

toText :: Constraint -> Text
toText c = case c of
  CAny -> "*"
  CLt vc -> "<" <> SemVer.toText vc
  CLtEq vc -> "<=" <> SemVer.toText vc
  CGt vc -> ">" <> SemVer.toText vc
  CGtEq vc -> ">=" <> SemVer.toText vc
  CEq vc -> "=" <> SemVer.toText vc
  CAnd a b -> toText a <> " " <> toText b
  COr a b -> toText a <> " || " <> toText b

--
-- Advanced syntax is not yet supported.
fromText :: Text -> Either String Constraint
fromText = Parsec.parseOnly parser

parser :: Parser Constraint
parser = parserWith Delimited.semantic

parserWith :: Delimiters -> Parser Constraint
parserWith delims =
  Parsec.choice ((<* Parsec.endOfInput) <$> [primP, andP, orP])
  where
    versionP =
      Delimited.parser delims False

    primP =
      Parsec.choice
        [ Parsec.char '*' *> pure CAny,
          Parsec.char '<' *> (CLt <$> versionP),
          Parsec.string "<=" *> (CLtEq <$> versionP),
          Parsec.char '>' *> (CGt <$> versionP),
          Parsec.string ">=" *> (CGtEq <$> versionP),
          CEq <$> ((Parsec.option '=' (Parsec.char '=')) *> versionP)
        ]

    andP =
      CAnd
        <$> primP
        <*> ( Parsec.skipSpace
                *> (andP <|> primP)
            )

    orP =
      COr
        <$> (andP <|> primP)
        <*> ( Parsec.skipSpace
                *> Parsec.string "||"
                *> Parsec.skipSpace
                *> (orP <|> andP <|> primP)
            )
