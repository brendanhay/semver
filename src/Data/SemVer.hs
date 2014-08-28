{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- Module      : Data.SemVer
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- in the specification are defined alongside additional lenses,
-- common manipulations, and serialisation primitives.
module Data.SemVer
    (
    -- * Version
      Identifier (..)
    , Version    (..)
    , defaultVersion

    -- ** Lenses
    , versionMajor
    , versionMinor
    , versionPatch
    , versionRelease
    , versionMeta

    -- ** Incrementing
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

    -- * Delimiters
    , Delimiters (..)
    , defaultDelimiters

    -- ** Lenses
    , delimMinor
    , delimPatch
    , delimRelease
    , delimMeta
    , delimIdent

    -- ** Encoding
    , toDelimitedBuilder

    -- ** Decoding
    , delimitedParser
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Function              (on)
import           Data.List                  (intersperse)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Prelude                    hiding (takeWhile)

-- | A type representing an individual identifier from the release
-- or metadata components of a 'Version'.
--
-- * The 'Ord' instance implements precedence according to the semantic version
-- specification, with numeric identifiers being of /lower/ precedence than
-- textual identifiers, otherwise lexicographic ordering is used.
data Identifier
    = INum  !Int
    | IText !Text
      deriving (Eq, Read, Show, Generic, Typeable)

instance Ord Identifier where
    compare a b = case (a, b) of
        (INum  x, INum  y) -> x `compare` y
        (IText x, IText y) -> x `compare` y
        (INum  _, _)       -> LT
        (IText _, _)       -> GT

instance NFData Identifier where
    rnf (INum  n) = rnf n
    rnf (IText t) = rnf t

-- | A type representing a successfully decoded or constructed semantic version.
--
-- * The 'Eq' instance represents exhaustive equality with all
-- components considered.
--
-- * The 'Ord' instance implements the precedence rules from the semantic
-- version specification with metadata ('_versionMeta') being ignored.
data Version = Version
    { _versionMajor   :: !Int
     -- ^ The major version component.
    , _versionMinor   :: !Int
      -- ^ The minor version component.
    , _versionPatch   :: !Int
      -- ^ The patch level component.
    , _versionRelease :: [Identifier]
      -- ^ A (potentially empty) list of release identifiers.
    , _versionMeta    :: [Identifier]
      -- ^ A (potentially empty) list of metadata.
    } deriving (Eq, Read, Show, Generic, Typeable)

-- | A default 'Version' which can be used to signify initial development.
--
-- Note: Equivalent to @0.0.0@
defaultVersion :: Version
defaultVersion = Version 0 0 0 [] []

instance Ord Version where
    compare a b = on compare versions a b <> on compare _versionRelease a b
      where
        versions Version{..} =
            [ _versionMajor
            , _versionMinor
            , _versionPatch
            ]

instance NFData Version where
    rnf Version{..} =
              rnf _versionMajor
        `seq` rnf _versionMinor
        `seq` rnf _versionPatch
        `seq` rnf _versionRelease
        `seq` rnf _versionMeta

versionMajor :: Functor f => (Int -> f Int) -> Version -> f Version
versionMajor f x = (\y -> x { _versionMajor = y }) <$> f (_versionMajor x)
{-# INLINE versionMajor #-}

versionMinor :: Functor f => (Int -> f Int) -> Version -> f Version
versionMinor f x = (\y -> x { _versionMinor = y }) <$> f (_versionMinor x)
{-# INLINE versionMinor #-}

versionPatch :: Functor f => (Int -> f Int) -> Version -> f Version
versionPatch f x = (\y -> x { _versionPatch = y }) <$> f (_versionPatch x)
{-# INLINE versionPatch #-}

versionRelease :: Functor f
               => ([Identifier] -> f [Identifier])
               -> Version
               -> f Version
versionRelease f x = (\y -> x { _versionRelease = y }) <$> f (_versionRelease x)
{-# INLINE versionRelease #-}

versionMeta :: Functor f
            => ([Identifier] -> f [Identifier])
            -> Version
            -> f Version
versionMeta f x = (\y -> x { _versionMeta = y }) <$> f (_versionMeta x)
{-# INLINE versionMeta #-}

-- | A set of delimiters used to encode/decode a 'Version' and specifyc
-- alternative serialisation strategies.
--
-- Example: using alpha characters to encode the version as a valid
-- DNS CNAME, such as:
--
-- @
-- let Right v = fromText "1.2.3+40"
-- let alpha   = Delimiters \'m\' \'p\' \'r\' \'d\' \'i\'
--
-- Data.Text.Lazy.Builder.toLazyText (\"app01-\" <> toDelimitedBuilder alpha v <> \".dmz.internal\")
-- @
--
-- Would result in the following 'LText.Text':
--
-- @
-- app01-1m2p3d40.dmz.internal
-- @
--
-- Using the same 'Delimiters' set with 'delimitedParser' would ensure
-- correct decoding behaviour.
data Delimiters = Delimiters
    { _delimMinor   :: !Char
    , _delimPatch   :: !Char
    , _delimRelease :: !Char
    , _delimMeta    :: !Char
    , _delimIdent   :: !Char
    } deriving (Eq, Ord, Read, Show, Generic, Typeable)

-- | The default set of delimiters used in the semantic version specification.
--
-- Example: Given exhaustive version components would result in the
-- following hypothetical version:
--
-- @
-- 1.2.3-alpha.1+sha.exp.12ab3d9
-- @
defaultDelimiters :: Delimiters
defaultDelimiters = Delimiters
    { _delimMinor   = '.'
    , _delimPatch   = '.'
    , _delimRelease = '-'
    , _delimMeta    = '+'
    , _delimIdent   = '.'
    }

instance NFData Delimiters where
    rnf Delimiters{..} =
              rnf _delimMinor
        `seq` rnf _delimPatch
        `seq` rnf _delimRelease
        `seq` rnf _delimMeta
        `seq` rnf _delimIdent

delimMinor :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimMinor f x = (\y -> x { _delimMinor = y }) <$> f (_delimMinor x)
{-# INLINE delimMinor #-}

delimPatch :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimPatch f x = (\y -> x { _delimPatch = y }) <$> f (_delimPatch x)
{-# INLINE delimPatch #-}

delimRelease :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimRelease f x = (\y -> x { _delimRelease = y }) <$> f (_delimRelease x)
{-# INLINE delimRelease #-}

delimMeta :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimMeta f x = (\y -> x { _delimMeta = y }) <$> f (_delimMeta x)
{-# INLINE delimMeta #-}

delimIdent :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimIdent f x = (\y -> x { _delimIdent = y }) <$> f (_delimIdent x)
{-# INLINE delimIdent #-}

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

-- | Check if the 'Version' is considered unstable.
--
-- * Major version zero (0.y.z) is for initial development.
--
-- * Anything may change at any time.
--
-- * The public API should not be considered stable.
isDevelopment :: Version -> Bool
isDevelopment = (== 0) . _versionMajor

-- | Check if the 'Version' is considered stable.
--
-- Version 1.0.0 defines the public API. The way in which the version number
-- is incremented after this release is dependent on this public API and how
-- it changes.
isPublic :: Version -> Bool
isPublic = (>= 1) . _versionMajor

-- | Convert a 'Version' to it's readable 'String' representation.
--
-- Note: This is optimised for cases where you wish to use a 'String' and
-- as such is faster than the semantically equivalent @unpack . toLazyText@.
toString :: Version -> String
toString = toMonoid (:[]) show Text.unpack defaultDelimiters

-- | Convert a 'Version' to a strict 'Text' representation.
--
-- Note: Equivalent to @toStrict . toLazyText@
toText :: Version -> Text
toText = LText.toStrict . toLazyText

-- | Convert a 'Version' to a 'LText.Text' representation.
--
-- Note: This uses a lower 'Builder' buffer size optimised for commonly
-- found version formats. If you have particuarly long version numbers
-- using 'toBuilder' and 'Build.toLazyTextWith' to control the buffer size
-- is recommended.
toLazyText :: Version -> LText.Text
toLazyText = Build.toLazyTextWith 24 . toBuilder

-- | Convert a 'Version' to a 'Builder'.
toBuilder :: Version -> Builder
toBuilder = toDelimitedBuilder defaultDelimiters

-- | Convert a 'Version' to a 'Builder' using the specified 'Delimiters' set.
toDelimitedBuilder :: Delimiters -> Version -> Builder
toDelimitedBuilder = toMonoid Build.singleton Build.decimal Build.fromText

toMonoid :: Monoid m
         => (Char -> m)
         -> (Int  -> m)
         -> (Text -> m)
         -> Delimiters
         -> Version
         -> m
toMonoid del int txt Delimiters{..} Version{..} = mconcat
     [ int _versionMajor
     , del _delimMinor
     , int _versionMinor
     , del _delimPatch
     , int _versionPatch
     , f _delimRelease _versionRelease
     , f _delimMeta    _versionMeta
     ]
  where
    f _ [] = mempty
    f c xs = del c <> mconcat (intersperse (del _delimIdent) (map g xs))

    g (INum  n) = int n
    g (IText t) = txt t
{-# INLINE toMonoid #-}

-- | Parse a 'Version' from 'Text', returning an attoparsec error message
-- in the 'Left' case on failure.
fromText :: Text -> Either String Version
fromText = parseOnly parser

-- | Parse a 'Version' from 'LText.Text', returning an attoparsec error message
-- in the 'Left' case on failure.
--
-- Note: The underlying attoparsec 'Parser' is based on 'Text' and this is
-- equivalent to @fromText . toStrict@
fromLazyText :: LText.Text -> Either String Version
fromLazyText = fromText . LText.toStrict

-- | A greedy attoparsec 'Parser' which requires the entire 'Text' input to match.
parser :: Parser Version
parser = delimitedParser defaultDelimiters

-- | A greedy attoparsec 'Parser' using the specified 'Delimiters' set
-- which requires the entire 'Text' input to match.
delimitedParser :: Delimiters -> Parser Version
delimitedParser Delimiters{..} = Version
    <$> (nonNegative <* char _delimMinor)
    <*> (nonNegative <* char _delimPatch)
    <*> nonNegative
    <*> option [] (try (char _delimRelease) *> identifiers)
    <*> option [] (try (char _delimMeta)    *> identifiers)
    <*  endOfInput
  where
    nonNegative :: (Show a, Integral a) => Parser a
    nonNegative = do
        n <- decimal
        when (n < 0) $
            fail ("Numeric value must be non-negative: " ++ show n)
        return n

    identifiers :: Parser [Identifier]
    identifiers = many (num <|> text)

    num = INum
        <$> nonNegative
        <*  (void (char _delimIdent) <|> endOfInput)

    text = IText
        <$> takeWhile1 (inClass "0-9A-Za-z-")
        <*  optional (char _delimIdent)
