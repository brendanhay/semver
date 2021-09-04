{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Applicative (many, optional, (<**>), (<|>))
import qualified Control.Monad as Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.SemVer as SemVer
import qualified Data.SemVer.Delimited as Delimited
import Data.SemVer.Internal
import Data.Text (Text)
import GHC.Stack (HasCallStack)

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

-- | Parsing function to create a 'Constraint' from 'Text' according to the rules specified
-- here: https://github.com/npm/node-semver#ranges
--
-- Advanced syntax is supported with the following conservative caveats:
-- * Wildcards can't appear in the middle of a fully specified version number.
-- That is: @1.x.x@ is valid as is @1.x@ but @1.x.1@ isn't.
-- * Prereleases and metadata information aren't allowed together with
-- wildcards. To use them you must specify the entire version number.
-- * The constraint is required to contain at least one part. That is @""@ is
-- not a valid constraint. Instead use @"*"@, @"X"@, or @"x"@ all of which
-- desugar to @CAny@
-- * Combinations of @~@ and @^@ with comparison operators (@>@, @<@, @<=@,
-- @>=@, @=@) isn't supported because their semantics is ill-defined; these
-- will fail to parse
fromText :: Text -> Either String Constraint
fromText = Parsec.parseOnly parser

parser :: Parser Constraint
parser = parserWith Delimited.semantic

-- | Partial versions found in ranges that may not contain every part.
-- Invariant:
-- * if major is Nothing then so are minor, patch, release, and meta
-- * likewise if minor is Nothing then so are patch, release and meta
data PartialVersion = PartialVersion
  { _partialVersionMajor :: Maybe Int,
    _partialVersionMinor :: Maybe Int,
    _partialVersionPatch :: Maybe Int,
    _partialVersionRelease :: [Identifier],
    _partialVersionMeta :: [Identifier]
  }
  deriving (Show)

smallestMatchingVersion :: PartialVersion -> Version
smallestMatchingVersion PartialVersion {..} =
  Version
    (fromMaybe 0 _partialVersionMajor)
    (fromMaybe 0 _partialVersionMinor)
    (fromMaybe 0 _partialVersionPatch)
    _partialVersionRelease
    _partialVersionMeta

-- | Get the next verison doesn't overlap the partial
-- Invariant: PartialVersion has major version of Just; there isn't a version
-- that doesn't overlap with @PartialVersion Nothing Nothing Nothing [] []@
nextNonMatchingVersion :: HasCallStack => PartialVersion -> Version
nextNonMatchingVersion PartialVersion {..} =
  case (_partialVersionMajor, _partialVersionMinor, _partialVersionPatch) of
    (Nothing, _, _) ->
      error "broken invariant: _partialVersionMajor must be Just"
    (Just m, Nothing, _) ->
      Version (m + 1) 0 0 [] []
    (Just m, Just n, Nothing) ->
      Version m (n + 1) 0 [] []
    (Just n, Just m, Just o) ->
      Version m n (o + 1) [] []

strictlyLessThan :: PartialVersion -> Constraint
strictlyLessThan = CLt . smallestMatchingVersion

lessThanOrEqual :: PartialVersion -> Constraint
lessThanOrEqual v@PartialVersion {..} =
  case (_partialVersionMajor, _partialVersionMinor, _partialVersionPatch) of
    (Nothing, Nothing, Nothing) -> CAny
    (Just major, Just minor, Just patch) ->
      CLtEq $ Version major minor patch _partialVersionRelease _partialVersionMeta
    _ -> CLt $ nextNonMatchingVersion v

strictlyGreaterThan :: PartialVersion -> Constraint
strictlyGreaterThan v@PartialVersion {..} =
  case (_partialVersionMajor, _partialVersionMinor, _partialVersionPatch) of
    (Nothing, Nothing, Nothing) -> CLt $ Version 0 0 0 [] []
    (Just major, Just minor, Just patch) ->
      CGt $ Version major minor patch _partialVersionRelease _partialVersionMeta
    _ -> CGtEq $ nextNonMatchingVersion v

greaterThanOrEqual :: PartialVersion -> Constraint
greaterThanOrEqual = CGtEq . smallestMatchingVersion

rangeBetweenPartialVersions :: PartialVersion -> PartialVersion -> Constraint
rangeBetweenPartialVersions minVersion maxVersion =
  CAnd (greaterThanOrEqual minVersion) (lessThanOrEqual maxVersion)

equalTo :: PartialVersion -> Constraint
equalTo v@PartialVersion {..} =
  case (_partialVersionMajor, _partialVersionMinor, _partialVersionPatch) of
    (Nothing, _, _) -> CAny
    (Just major, Just minor, Just patch) ->
      CEq $ Version major minor patch _partialVersionRelease _partialVersionMeta
    _ -> rangeBetweenPartialVersions v v

tildedVersionRange :: PartialVersion -> Constraint
tildedVersionRange v@PartialVersion {..} = case _partialVersionMinor of
  Just _ ->
    rangeBetweenPartialVersions
      v
      (PartialVersion _partialVersionMajor _partialVersionMinor Nothing [] [])
  Nothing ->
    equalTo $ PartialVersion _partialVersionMajor Nothing Nothing [] []

caretedVersionRange :: PartialVersion -> Constraint
caretedVersionRange v@PartialVersion {..} =
  case (_partialVersionMajor, _partialVersionMinor, _partialVersionPatch) of
    (Just 0, Just 0, Just n) ->
      -- To properly handle release versions we need to manually construct
      -- this range so that releases are included. With
      -- rangeBetweenPartialVersions we would generate @>=0.0.n-alpha.1 <=0.0.n@
      -- which would not match @0.0.n-alpha.2@ for example, but here we construct
      -- @>=0.0.n-alpha.1 <0.0.(n + 1)@ which does match @0.0.n-alpha.2@
      -- This isn't a problem for the other cases because we already generate
      -- something with @<@ for the upper bound.
      CAnd
        (greaterThanOrEqual v)
        (CLt (Version 0 0 (n + 1) [] []))
    (Just 0, Just 0, _) ->
      rangeBetweenPartialVersions
        v
        (PartialVersion _partialVersionMajor _partialVersionMinor _partialVersionPatch [] [])
    (Just 0, _, _) ->
      rangeBetweenPartialVersions
        v
        (PartialVersion _partialVersionMajor _partialVersionMinor Nothing [] [])
    _ ->
      rangeBetweenPartialVersions
        v
        (PartialVersion _partialVersionMajor Nothing Nothing [] [])

parserWith :: Delimiters -> Parser Constraint
parserWith Delimiters {..} = rangeSet <* Parsec.endOfInput
  where
    -- implementation of these follows fairly closely to the Range Grammar
    -- specified here: https://github.com/npm/node-semver#range-grammar
    rangeSet = List.foldl1' COr <$> range `Parsec.sepBy1'` logicalOr
    logicalOr = Parsec.skipSpace *> Parsec.string "||" *> Parsec.skipSpace
    range =
      (primitive <**> moreSimples)
        <|> (tilde <**> moreSimples)
        <|> (caret <**> moreSimples)
        <|> ( (partial <* Parsec.char ' ' <* Parsec.skipSpace)
                <**> (hyphenEnd <|> fmap (. equalTo) moreSimples)
            )
        <|> (equalTo <$> partial)
    moreSimples :: Parser (Constraint -> Constraint)
    moreSimples =
      flip (\first -> List.foldl1' CAnd . (first :))
        <$> Parsec.many' (Parsec.skipSpace *> simple)
    hyphenEnd :: Parser (PartialVersion -> Constraint)
    hyphenEnd =
      flip rangeBetweenPartialVersions
        <$> (Parsec.string "- " *> Parsec.skipSpace *> partial)
    simple :: Parser Constraint
    simple = primitive <|> tilde <|> caret <|> (equalTo <$> partial)
    primitive =
      Parsec.choice
        [ Parsec.char '<' *> Parsec.skipSpace *> (strictlyLessThan <$> partial),
          Parsec.string "<=" *> Parsec.skipSpace *> (lessThanOrEqual <$> partial),
          Parsec.char '>' *> Parsec.skipSpace *> (strictlyGreaterThan <$> partial),
          Parsec.string ">=" *> Parsec.skipSpace *> (greaterThanOrEqual <$> partial),
          Parsec.char '=' *> Parsec.skipSpace *> (equalTo <$> partial)
        ]
    tilde = Parsec.char '~' *> Parsec.skipSpace *> (tildedVersionRange <$> partial)
    caret = Parsec.char '^' *> Parsec.skipSpace *> (caretedVersionRange <$> partial)
    versionPart :: Parser (Maybe Int)
    versionPart =
      (Nothing <$ Parsec.satisfy (Parsec.inClass "xX*"))
        <|> (Just <$> nonNegative)
    partial :: Parser PartialVersion
    partial = do
      major <- versionPart
      minor <- optional (Parsec.char _delimMinor *> versionPart)
      patch <- optional (Parsec.char _delimPatch *> versionPart)
      release <-
        Parsec.option [] (Parsec.try (Parsec.char _delimRelease) *> identifiers)
      meta <-
        Parsec.option [] (Parsec.try (Parsec.char _delimMeta) *> identifiers)
      case (major, minor, patch, release, meta) of
        (_, _, Nothing, _ : _, _) ->
          fail "can't supply prerelease without supplying patch"
        (_, _, Just Nothing, _ : _, _) ->
          fail "can't supply prerelease without supplying explicit patch"
        (_, _, Nothing, _, _ : _) ->
          fail "can't supply metadata without supplying patch"
        (_, _, Just Nothing, _, _ : _) ->
          fail "can't supply metadata without supplying explicit patch"
        (_, Nothing, Just _, _, _) ->
          fail "can't supply patch without supplying minor"
        (_, Just Nothing, Just (Just _), _, _) ->
          fail "can't supply specific patch after supplying wildcard for minor"
        (Nothing, Just (Just _), _, _, _) ->
          fail "can't supply specific minor after supplying wildcard for major"
        (Nothing, Nothing, Nothing, _, _) ->
          pure $ PartialVersion Nothing Nothing Nothing release meta
        (_, Nothing, Nothing, _, _) ->
          pure $ PartialVersion major Nothing Nothing release meta
        (_, Just minor', Nothing, _, _) ->
          pure $ PartialVersion major minor' Nothing release meta
        (_, Just minor', Just patch', _, _) ->
          pure $ PartialVersion major minor' patch' release meta
    identifiers :: Parser [Identifier]
    identifiers = many (identifierParser $ Monad.void (Parsec.char _delimIdent))
