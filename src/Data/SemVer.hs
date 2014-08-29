{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
    -- $lenses
    , versionMajor
    , versionMinor
    , versionPatch
    , versionRelease
    , versionMeta
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

    -- * Delimiters
    -- $delimiters
    , Delimiters
    -- ** Constructor
    , delimiters
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
import           Prelude                    hiding (takeWhile)

-- | A type representing an individual identifier from the release
-- or metadata components of a 'Version'.
--
-- * The 'Ord' instance implements precedence according to the semantic version
-- specification, with numeric identifiers being of /lower/ precedence than
-- textual identifiers, otherwise lexicographic ordering is used.
--
-- The functions 'numeric' and 'textual' can be used to construct an 'Identifier'.
data Identifier
    = INum  !Int
    | IText !Text
      deriving (Eq, Show)

-- | Safely construct a numeric identifier.
numeric :: Int -> Identifier
numeric = INum
{-# INLINE numeric #-}

-- | Construct an identifier from the given 'Text', returning 'Nothing' if
-- neither a numeric or valid textual input is supplied.
textual :: Text -> Maybe Identifier
textual = either (const Nothing) Just
    . parseOnly (identifierParser endOfInput <* endOfInput)
{-# INLINE textual #-}

_Numeric :: Applicative f => (Int -> f Int) -> Identifier -> f Identifier
_Numeric f (INum x) = INum <$> f x
_Numeric _ x        = pure x
{-# INLINE _Numeric #-}

_Textual :: Applicative f => (Text -> f Text) -> Identifier -> f Identifier
_Textual f (IText x) = IText <$> f x
_Textual _ x         = pure x
{-# INLINE _Textual #-}

instance Ord Identifier where
    compare a b = case (a, b) of
        (INum  x, INum  y) -> x `compare` y
        (IText x, IText y) -> x `compare` y
        (INum  _, _)       -> LT
        (IText _, _)       -> GT

instance NFData Identifier where
    rnf (INum  n) = rnf n
    rnf (IText t) = rnf t

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
    { _versionMajor   :: !Int
    , _versionMinor   :: !Int
    , _versionPatch   :: !Int
    , _versionRelease :: [Identifier]
    , _versionMeta    :: [Identifier]
    } deriving (Eq, Show)

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

-- $lenses
--
-- Something about using lenses to construct versions and increments to update
-- the version components due to the modification of all version according to spec

-- | Lens for the major version component.
versionMajor :: Functor f => (Int -> f Int) -> Version -> f Version
versionMajor f x = (\y -> x { _versionMajor = y }) <$> f (_versionMajor x)
{-# INLINE versionMajor #-}

-- | Lens for minor version component.
versionMinor :: Functor f => (Int -> f Int) -> Version -> f Version
versionMinor f x = (\y -> x { _versionMinor = y }) <$> f (_versionMinor x)
{-# INLINE versionMinor #-}

-- | Lens for the patch version component.
versionPatch :: Functor f => (Int -> f Int) -> Version -> f Version
versionPatch f x = (\y -> x { _versionPatch = y }) <$> f (_versionPatch x)
{-# INLINE versionPatch #-}

-- | Lens for the list of release identifiers.
versionRelease :: Functor f
               => ([Identifier] -> f [Identifier])
               -> Version
               -> f Version
versionRelease f x = (\y -> x { _versionRelease = y }) <$> f (_versionRelease x)
{-# INLINE versionRelease #-}

-- | Lens for the list of metadata identifiers.
versionMeta :: Functor f
            => ([Identifier] -> f [Identifier])
            -> Version
            -> f Version
versionMeta f x = (\y -> x { _versionMeta = y }) <$> f (_versionMeta x)
{-# INLINE versionMeta #-}

-- $delimiters
--
-- A set of delimiters is used to encode/decode a 'Version' and specify
-- alternative serialisation strategies.
--
-- Lenses can be used to modify the default delimiter set, as in the following
-- example - using alpha characters to encode the version as a valid
-- DNS CNAME:
--
-- @
-- let Right v = fromText "1.2.3+40"
-- let alpha = delimiters & delimMajor .= \'m\' & delimPatch .= \'p\' & delimRelease .= \'r\' & delimMeta .= \'d\' & delimIdent .= \'i\'
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

-- | An opaque set representing the seperators used to delimit semantic
-- version components.
data Delimiters = Delimiters
    { _delimMinor   :: !Char
    , _delimPatch   :: !Char
    , _delimRelease :: !Char
    , _delimMeta    :: !Char
    , _delimIdent   :: !Char
    } deriving (Eq, Ord, Show)

-- | The default set of delimiters used in the semantic version specification.
--
-- Example: Given exhaustive version components would result in the
-- following hypothetical version:
--
-- @
-- 1.2.3-alpha.1+sha.exp.12ab3d9
-- @
delimiters :: Delimiters
delimiters = Delimiters
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

-- | Lens for the minor version delimiter. Default: @.@
delimMinor :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimMinor f x = (\y -> x { _delimMinor = y }) <$> f (_delimMinor x)
{-# INLINE delimMinor #-}

-- | Lens for the patch version delimiter. Default: @.@
delimPatch :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimPatch f x = (\y -> x { _delimPatch = y }) <$> f (_delimPatch x)
{-# INLINE delimPatch #-}

-- | Lens for the release component delimiter. Default: @-@
delimRelease :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimRelease f x = (\y -> x { _delimRelease = y }) <$> f (_delimRelease x)
{-# INLINE delimRelease #-}

-- | Lens for the metadata component delimiter. Default: @+@
delimMeta :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimMeta f x = (\y -> x { _delimMeta = y }) <$> f (_delimMeta x)
{-# INLINE delimMeta #-}

-- | Lens for the individual identifier delimiter. Default: @.@
delimIdent :: Functor f => (Char -> f Char) -> Delimiters -> f Delimiters
delimIdent f x = (\y -> x { _delimIdent = y }) <$> f (_delimIdent x)
{-# INLINE delimIdent #-}

-- $increments
--
-- Mention the usage of increments to correctly adjust version components
-- according to spec.

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
toString = toMonoid (:[]) show Text.unpack delimiters

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
toBuilder = toDelimitedBuilder delimiters

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
parser = delimitedParser delimiters

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
    identifiers :: Parser [Identifier]
    identifiers = many (identifierParser $ void (char _delimIdent))

identifierParser :: Parser () -> Parser Identifier
identifierParser end = num <|> text
  where
    num  = INum  <$> nonNegative <* (end <|> endOfInput)
    text = IText <$> takeWhile1 (inClass "0-9A-Za-z-") <* optional end

nonNegative :: (Show a, Integral a) => Parser a
nonNegative = do
    n <- decimal
    when (n < 0) $
        fail ("Numeric value must be non-negative: " ++ show n)
    return n
