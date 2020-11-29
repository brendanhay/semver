{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Data.SemVer.Internal
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
module Data.SemVer.Internal where

import Control.Applicative (optional, (<|>))
import Control.DeepSeq (NFData)
import qualified Control.Monad as Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Function as Function
import Data.Hashable (Hashable)
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import GHC.Generics (Generic)

-- | An opaque type representing a successfully decoded or constructed
-- semantic version. See the related functions and lenses for modification and
-- update.
--
-- * The 'Eq' instance represents exhaustive equality with all
-- components considered.
--
-- * The 'Ord' instance implements the precedence rules from the semantic
-- version specification with metadata being ignored.
data Version = Version
  { _versionMajor :: !Int,
    _versionMinor :: !Int,
    _versionPatch :: !Int,
    _versionRelease :: [Identifier],
    _versionMeta :: [Identifier]
  }
  deriving (Eq, Show, Generic)

instance Ord Version where
  compare a b = Function.on compare versions a b <> release
    where
      versions Version {..} =
        [ _versionMajor,
          _versionMinor,
          _versionPatch
        ]
      release =
        case (_versionRelease a, _versionRelease b) of
          ([], _ : _) -> GT
          (_ : _, []) -> LT
          (x, y) -> x `compare` y

instance NFData Version

instance Hashable Version

-- | A type representing an individual identifier from the release
-- or metadata components of a 'Version'.
--
-- * The 'Ord' instance implements precedence according to the semantic version
-- specification, with numeric identifiers being of /lower/ precedence than
-- textual identifiers, otherwise lexicographic ordering is used.
--
-- The functions 'numeric' and 'textual' can be used to construct an 'Identifier'.
data Identifier
  = INum !Int
  | IText !Text
  deriving (Eq, Show, Generic)

instance Ord Identifier where
  compare a b = case (a, b) of
    (INum x, INum y) -> x `compare` y
    (IText x, IText y) -> x `compare` y
    (INum _, _) -> LT
    (IText _, _) -> GT

instance NFData Identifier

instance Hashable Identifier

identifierParser :: Parser () -> Parser Identifier
identifierParser p =
  either INum IText
    <$> Parsec.eitherP (numericParser p) (textualParser p)

numericParser :: Parser () -> Parser Int
numericParser p =
  nonNegative <* (p <|> Parsec.endOfInput)

textualParser :: Parser () -> Parser Text
textualParser p =
  Parsec.takeWhile1 (Parsec.inClass "0-9A-Za-z-") <* optional p

nonNegative :: (Show a, Integral a) => Parser a
nonNegative = do
  n <- Parsec.decimal

  Monad.when (n < 0) $
    fail ("Numeric value must be non-negative: " ++ show n)

  pure n

-- | An opaque set representing the seperators used to delimit semantic
-- version components.
data Delimiters = Delimiters
  { _delimMinor :: !Char,
    _delimPatch :: !Char,
    _delimRelease :: !Char,
    _delimMeta :: !Char,
    _delimIdent :: !Char
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData Delimiters

instance Hashable Delimiters

toMonoid ::
  Monoid m =>
  (Char -> m) ->
  (Int -> m) ->
  (Text -> m) ->
  Delimiters ->
  Version ->
  m
toMonoid del int txt Delimiters {..} Version {..} =
  mconcat
    [ int _versionMajor,
      del _delimMinor,
      int _versionMinor,
      del _delimPatch,
      int _versionPatch,
      f _delimRelease _versionRelease,
      f _delimMeta _versionMeta
    ]
  where
    f _ [] = Monoid.mempty
    f c xs =
      del c
        `Monoid.mappend` Monoid.mconcat (List.intersperse (del _delimIdent) (map g xs))

    g (INum n) = int n
    g (IText t) = txt t
{-# INLINE toMonoid #-}
