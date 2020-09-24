-- Module      : Data.SemVer
-- Copyright   : (c) 2014-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | An implementation of the Semantic Versioning specification located at
-- <http://semver.org>.
--
-- A canonical 'Version' type and functions representing behaviour as outlined
-- in the specification are defined alongside additional lenses, traversals,
-- common manipulations, and serialisation primitives.
module Data.SemVer
    (
    -- * Version
      Version
    -- ** Constructors
    , version
    , initial
    -- ** Lenses
    , major
    , minor
    , patch
    , release
    , metadata
    -- ** Incrementing
    -- $incrementing
    , incrementMajor
    , incrementMinor
    , incrementPatch
    -- ** Predicates
    , isDevelopment
    , isPublic
    -- ** Encoding
    , toString
    , toText
    , toLazyText
    , toBuilder
    -- ** Decoding
    , fromText
    , fromLazyText
    , parser

    -- * Identifiers
    , Identifier
    -- ** Constructors
    , numeric
    , textual
    -- ** Prisms
    , _Numeric
    , _Textual
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.SemVer.Delimited  as Delim
import           Data.SemVer.Internal
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Build

-- | Smart constructor fully specifying all available version components.
version :: Int          -- ^ Major version component.
        -> Int          -- ^ Minor version component.
        -> Int          -- ^ Patch version component.
        -> [Identifier] -- ^ Release identifiers.
        -> [Identifier] -- ^ Metadata identifiers.
        -> Version
version = Version
{-# INLINE version #-}

-- | A default 'Version' which can be used to signify initial development.
--
-- Note: Equivalent to @0.0.0@
initial :: Version
initial = version 0 0 0 [] []

-- | Lens for the major version component.
major :: Functor f => (Int -> f Int) -> Version -> f Version
major f x = (\y -> x { _versionMajor = y }) <$> f (_versionMajor x)
{-# INLINE major #-}

-- | Lens for minor version component.
minor :: Functor f => (Int -> f Int) -> Version -> f Version
minor f x = (\y -> x { _versionMinor = y }) <$> f (_versionMinor x)
{-# INLINE minor #-}

-- | Lens for the patch version component.
patch :: Functor f => (Int -> f Int) -> Version -> f Version
patch f x = (\y -> x { _versionPatch = y }) <$> f (_versionPatch x)
{-# INLINE patch #-}

-- | Lens for the list of release identifiers.
release :: Functor f
        => ([Identifier] -> f [Identifier])
        -> Version
        -> f Version
release f x = (\y -> x { _versionRelease = y }) <$> f (_versionRelease x)
{-# INLINE release #-}

-- | Lens for the list of metadata identifiers.
metadata :: Functor f
         => ([Identifier] -> f [Identifier])
         -> Version
         -> f Version
metadata f x = (\y -> x { _versionMeta = y }) <$> f (_versionMeta x)
{-# INLINE metadata #-}

-- $incrementing
--
-- The following increment functions are used to ensure that the related
-- version components are reset according to the specification.
--
-- See the individual function documentation for specifics regarding each
-- version component.

-- | Increment the major component of a 'Version' by 1, resetting the minor
-- and patch components.
--
-- * Major version X (X.y.z | X > 0) MUST be incremented if any backwards
-- incompatible changes are introduced to the public API.
--
-- * It MAY include minor and patch level changes.
--
-- * Patch and minor version MUST be reset to 0 when major version
-- is incremented.
incrementMajor :: Version -> Version
incrementMajor v = v
    { _versionMajor = _versionMajor v + 1
    , _versionMinor = 0
    , _versionPatch = 0
    }
{-# INLINE incrementMajor #-}

-- | Increment the minor component of a 'Version' by 1, resetting the
-- patch component.
--
-- * Minor version Y (x.Y.z | x > 0) MUST be incremented if new, backwards
-- compatible functionality is introduced to the public API.
--
-- * It MUST be incremented if any public API functionality is marked
-- as deprecated.
--
-- * It MAY be incremented if substantial new functionality or improvements
-- are introduced within the private code.
--
-- * It MAY include patch level changes.
--
-- * Patch version MUST be reset to 0 when minor version is incremented.
incrementMinor :: Version -> Version
incrementMinor v = v
    { _versionMinor = _versionMinor v + 1
    , _versionPatch = 0
    }
{-# INLINE incrementMinor #-}

-- | Increment the patch component of a 'Version' by 1.
--
-- * Patch version Z (x.y.Z | x > 0) MUST be incremented if only backwards
-- compatible bug fixes are introduced.
--
-- * A bug fix is defined as an internal change that fixes incorrect behavior.
incrementPatch :: Version -> Version
incrementPatch v = v
    { _versionPatch = _versionPatch v + 1
    }
{-# INLINE incrementPatch #-}

-- | Check if the 'Version' is considered unstable.
--
-- * Major version zero (0.y.z) is for initial development.
--
-- * Anything may change at any time.
--
-- * The public API should not be considered stable.
isDevelopment :: Version -> Bool
isDevelopment = (== 0) . _versionMajor
{-# INLINE isDevelopment #-}

-- | Check if the 'Version' is considered stable.
--
-- Version 1.0.0 defines the public API. The way in which the version number
-- is incremented after this release is dependent on this public API and how
-- it changes.
isPublic :: Version -> Bool
isPublic = (>= 1) . _versionMajor
{-# INLINE isPublic #-}

-- | Convert a 'Version' to it's readable 'String' representation.
--
-- Note: This is optimised for cases where you require 'String' output, and
-- as such is faster than the semantically equivalent @unpack . toLazyText@.
toString :: Version -> String
toString = toMonoid (:[]) show Text.unpack Delim.semantic
{-# INLINE toString #-}

-- | Convert a 'Version' to a strict 'Text' representation.
--
-- Note: Equivalent to @toStrict . toLazyText@
toText :: Version -> Text
toText = LText.toStrict . toLazyText
{-# INLINE toText #-}

-- | Convert a 'Version' to a 'LText.Text' representation.
--
-- Note: This uses a lower 'Builder' buffer size optimised for commonly
-- found version formats. If you have particuarly long version numbers
-- using 'toBuilder' and 'Build.toLazyTextWith' to control the buffer size
-- is recommended.
toLazyText :: Version -> LText.Text
toLazyText = Build.toLazyTextWith 24 . toBuilder
{-# INLINE toLazyText #-}

-- | Convert a 'Version' to a 'Builder'.
toBuilder :: Version -> Builder
toBuilder = Delim.toBuilder Delim.semantic
{-# INLINE toBuilder #-}

-- | Parse a 'Version' from 'Text', returning an attoparsec error message
-- in the 'Left' case on failure.
fromText :: Text -> Either String Version
fromText = parseOnly parser
{-# INLINE fromText #-}

-- | Parse a 'Version' from 'LText.Text', returning an attoparsec error message
-- in the 'Left' case on failure.
--
-- Note: The underlying attoparsec 'Parser' is based on 'Text' and this is
-- equivalent to @fromText . toStrict@
fromLazyText :: LText.Text -> Either String Version
fromLazyText = fromText . LText.toStrict
{-# INLINE fromLazyText #-}

-- | A greedy attoparsec 'Parser' which requires the entire 'Text'
-- input to match.
parser :: Parser Version
parser = Delim.parser Delim.semantic True
{-# INLINE parser #-}

-- | Safely construct a numeric identifier.
numeric :: Int -> Identifier
numeric = INum
{-# INLINE numeric #-}

-- | Construct an identifier from the given 'Text', returning 'Nothing' if
-- neither a numeric or valid textual input is supplied.
textual :: Text -> Maybe Identifier
textual = either (const Nothing) (Just . IText)
    . parseOnly (textualParser endOfInput <* endOfInput)
{-# INLINE textual #-}

-- | A prism into the numeric branch of an 'Identifier'.
_Numeric :: Applicative f => (Int -> f Int) -> Identifier -> f Identifier
_Numeric f (INum x) = INum <$> f x
_Numeric _ x        = pure x
{-# INLINE _Numeric #-}

-- | A prism into the textual branch of an 'Identifier'.
_Textual :: Applicative f => (Text -> f Text) -> Identifier -> f (Maybe Identifier)
_Textual f (IText x) = textual <$> f x
_Textual _ x         = pure (Just x)
{-# INLINE _Textual #-}
