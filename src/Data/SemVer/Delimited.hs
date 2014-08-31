{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Data.SemVer.Delimited
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | A set of delimiters can be used to encode/decode a 'Version' and specify
-- alternative serialisation strategies.
--
-- Lenses can be used to modify the default delimiter set, as in the following
-- example - using alpha characters to encode the version as a valid
-- DNS CNAME (assuming operators from lens or lens-family-core):
--
-- @
-- let Right v = fromText "1.2.3+40"
-- let alpha = semantic & major .~ \'m\' & patch .~ \'p\' & release .~ \'r\' & metadata .~ \'d\' & identifier .~ \'i\'
--
-- Data.Text.Lazy.Builder.toLazyText (\"app01-\" <> toBuilder alpha v <> \".dmz.internal\")
-- @
--
-- Would result in the following 'LText.Text':
--
-- @
-- app01-1m2p3d40.dmz.internal
-- @
--
-- Using the same 'Delimiters' set with 'parser' would ensure
-- correct decoding behaviour.
module Data.SemVer.Delimited
    (
    -- * Delimiters
      Delimiters
    -- ** Constructor
    , semantic
    -- ** Lenses
    , minor
    , patch
    , release
    , metadata
    , identifier
    -- ** Encoding
    , toBuilder
    -- ** Decoding
    , parser
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.SemVer.Internal
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build

-- | The default set of delimiters used in the semantic version specification.
--
-- Example: Given exhaustive version components would result in the
-- following hypothetical version:
--
-- @
-- 1.2.3-alpha.1+sha.exp.12ab3d9
-- @
semantic :: Delimiters
semantic = Delimiters
    { _delimMinor   = '.'
    , _delimPatch   = '.'
    , _delimRelease = '-'
    , _delimMeta    = '+'
    , _delimIdent   = '.'
    }

-- | Lens for the minor version delimiter. Default: @.@
minor :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
minor f x = (\y -> x { _delimMinor = y }) <$> f (_delimMinor x)
{-# INLINE minor #-}

-- | Lens for the patch version delimiter. Default: @.@
patch :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
patch f x = (\y -> x { _delimPatch = y }) <$> f (_delimPatch x)
{-# INLINE patch #-}

-- | Lens for the release component delimiter. Default: @-@
release :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
release f x = (\y -> x { _delimRelease = y }) <$> f (_delimRelease x)
{-# INLINE release #-}

-- | Lens for the metadata component delimiter. Default: @+@
metadata :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
metadata f x = (\y -> x { _delimMeta = y }) <$> f (_delimMeta x)
{-# INLINE metadata #-}

-- | Lens for the individual identifier delimiter. Default: @.@
identifier :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
identifier f x = (\y -> x { _delimIdent = y }) <$> f (_delimIdent x)
{-# INLINE identifier #-}

-- | Convert a 'Version' to a 'Builder' using the specified 'Delimiters' set.
toBuilder :: Delimiters -> Version -> Builder
toBuilder = toMonoid Build.singleton Build.decimal Build.fromText

-- | A greedy attoparsec 'Parser' using the specified 'Delimiters' set
-- which requires the entire 'Text' input to match.
parser :: Delimiters -> Parser Version
parser Delimiters{..} = Version
    <$> (nonNegative <* char _delimMinor)
    <*> (nonNegative <* char _delimPatch)
    <*> nonNegative
    <*> option [] (try (char _delimRelease)  *> identifiers)
    <*> option [] (try (char _delimMeta) *> identifiers)
    <*  endOfInput
  where
    identifiers :: Parser [Identifier]
    identifiers = many (identifierParser $ void (char _delimIdent))
