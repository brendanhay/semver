{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

-- Module      : Data.SemVer
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.SemVer
    (
    -- * Version
      Identifier (..)
    , Version    (..)
    -- ** Default constructor
    , defaultVersion
    -- ** Lenses
    , versionMajor
    , versionMinor
    , versionPatch
    , versionRelease
    , versionMeta

    -- * Delimiters
    , Delimiters (..)
    -- ** Default constructor
    -- ** Lenses
    , delimMinor
    , delimPatch
    , delimRelease
    , delimMeta
    , delimIdent

    -- * Incrementing
    , incrementMajor
    , incrementMinor
    , incrementPatch

    -- * Predicates
    , isDevelopment
    , isPublic

    -- * Serialising
    , toText
    , toBuilder
    , toDelimitedBuilder

    -- * Deserialising
    , fromText
    -- ** Attoparsec
    , parser
    , delimitedParser
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Foldable              (foldr')
import           Data.Function              (on)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Prelude                    hiding (takeWhile)

data Identifier
    = INum  !Integer
    | IText Text
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

data Version = Version
    { _versionMajor   :: !Int
    , _versionMinor   :: !Int
    , _versionPatch   :: !Int
    , _versionRelease :: [Identifier]
    , _versionMeta    :: [Identifier]
    } deriving (Eq, Read, Show, Generic, Typeable)

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

data Delimiters = Delimiters
    { _delimMinor   :: !Char
    , _delimPatch   :: !Char
    , _delimRelease :: !Char
    , _delimMeta    :: !Char
    , _delimIdent   :: !Char
    } deriving (Eq, Ord, Read, Show, Generic, Typeable)

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

-- | Major version X (X.y.z | X > 0) MUST be incremented if any backwards
-- incompatible changes are introduced to the public API.
--
-- It MAY include minor and patch level changes.
--
-- Patch and minor version MUST be reset to 0 when major version is incremented.
incrementMajor :: Version -> Version
incrementMajor v = v
    { _versionMajor = _versionMajor v + 1
    , _versionMinor = 0
    , _versionPatch = 0
    }
{-# INLINE incrementMajor #-}

-- | Minor version Y (x.Y.z | x > 0) MUST be incremented if new, backwards
-- compatible functionality is introduced to the public API.
--
-- It MUST be incremented if any public API functionality is marked as deprecated.
--
-- It MAY be incremented if substantial new functionality or improvements are
-- introduced within the private code.
--
-- It MAY include patch level changes.
--
-- Patch version MUST be reset to 0 when minor version is incremented.
incrementMinor :: Version -> Version
incrementMinor v = v
    { _versionMinor = _versionMinor v + 1
    , _versionPatch = 0
    }
{-# INLINE incrementMinor #-}

-- | Patch version Z (x.y.Z | x > 0) MUST be incremented if only backwards
-- compatible bug fixes are introduced.
--
-- A bug fix is defined as an internal change that fixes incorrect behavior.
incrementPatch :: Version -> Version
incrementPatch v = v
    { _versionPatch = _versionPatch v + 1
    }
{-# INLINE incrementPatch #-}

-- | Major version zero (0.y.z) is for initial development.
--
-- Anything may change at any time.
--
-- The public API should not be considered stable.
isDevelopment :: Version -> Bool
isDevelopment = (== 0) . _versionMajor
{-# INLINE isDevelopment #-}

-- | Version 1.0.0 defines the public API.
--
-- The way in which the version number is incremented after this release is
-- dependent on this public API and how it changes.
isPublic :: Version -> Bool
isPublic = (>= 1) . _versionMajor
{-# INLINE isPublic #-}

toText :: Version -> Text
toText = LText.toStrict . Build.toLazyText . toBuilder

toBuilder :: Version -> Builder
toBuilder = toDelimitedBuilder defaultDelimiters

toDelimitedBuilder :: Delimiters -> Version -> Builder
toDelimitedBuilder Delimiters{..} Version{..} =
       Build.decimal   _versionMajor
    <> Build.singleton _delimMinor
    <> Build.decimal   _versionMinor
    <> Build.singleton _delimPatch
    <> Build.decimal   _versionPatch
    <> f _delimRelease _versionRelease
    <> f _delimMeta    _versionMeta
  where
    f (Build.singleton -> x) = foldr' (mappend . mappend x . g) mempty

    g (INum  n) = Build.decimal  n
    g (IText t) = Build.fromText t

fromText :: Text -> Either String Version
fromText = parseOnly parser

-- | A greedy attoparsec parser which requires the entire input to match.
parser :: Parser Version
parser = delimitedParser defaultDelimiters

delimitedParser :: Delimiters -> Parser Version
delimitedParser Delimiters{..} = Version
    <$> (nonNegative <* char _delimMinor)
    <*> (nonNegative <* char _delimPatch)
    <*> nonNegative
    <*> option [] (try (char _delimRelease) *> identifiers)
    <*> option [] (try (char _delimMeta)    *> identifiers)
  where
    nonNegative :: (Show a, Integral a) => Parser a
    nonNegative = do
        n <- decimal
        when (n < 0) $
            fail ("Numeric value must be non-negative: " ++ show n)
        return n

    identifiers :: Parser [Identifier]
    identifiers = identifier `sepBy1` char _delimIdent

    identifier :: Parser Identifier
    identifier = either INum IText
        <$> eitherP nonNegative (takeWhile1 (inClass "0-9A-Za-z-"))
