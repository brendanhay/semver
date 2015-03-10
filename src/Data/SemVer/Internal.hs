{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Data.SemVer.Internal
-- Copyright   : (c) 2014-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.SemVer.Internal where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Attoparsec.Text
import Data.Function        (on)
import Data.List            (intersperse)
import Data.Monoid
import Data.Text            (Text)

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

instance Ord Version where
    compare a b = on compare versions a b <> on compareVersionRelease _versionRelease a b
      where
        versions Version{..} =
            [ _versionMajor
            , _versionMinor
            , _versionPatch
            ]

-- | Compare version releases.
--
-- Note: Contrary to 'List's, @[] `compare` [xs]@ equals to @GT@
compareVersionRelease :: [Identifier] -> [Identifier] -> Ordering
[] `compareVersionRelease` _  = GT
_  `compareVersionRelease` [] = LT
a  `compareVersionRelease` b  = compare a b

instance NFData Version where
    rnf Version{..} =
              rnf _versionMajor
        `seq` rnf _versionMinor
        `seq` rnf _versionPatch
        `seq` rnf _versionRelease
        `seq` rnf _versionMeta

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

instance Ord Identifier where
    compare a b = case (a, b) of
        (INum  x, INum  y) -> x `compare` y
        (IText x, IText y) -> x `compare` y
        (INum  _, _)       -> LT
        (IText _, _)       -> GT

instance NFData Identifier where
    rnf (INum  n) = rnf n
    rnf (IText t) = rnf t

identifierParser :: Parser () -> Parser Identifier
identifierParser p =
    either INum IText <$> eitherP (numericParser p) (textualParser p)

numericParser :: Parser () -> Parser Int
numericParser p = nonNegative <* (p <|> endOfInput)

textualParser :: Parser () -> Parser Text
textualParser p = takeWhile1 (inClass "0-9A-Za-z-") <* optional p

nonNegative :: (Show a, Integral a) => Parser a
nonNegative = do
    n <- decimal
    when (n < 0) $
        fail ("Numeric value must be non-negative: " ++ show n)
    return n

-- | An opaque set representing the seperators used to delimit semantic
-- version components.
data Delimiters = Delimiters
    { _delimMinor   :: !Char
    , _delimPatch   :: !Char
    , _delimRelease :: !Char
    , _delimMeta    :: !Char
    , _delimIdent   :: !Char
    } deriving (Eq, Ord, Show)

instance NFData Delimiters where
    rnf Delimiters{..} =
              rnf _delimMinor
        `seq` rnf _delimPatch
        `seq` rnf _delimRelease
        `seq` rnf _delimMeta
        `seq` rnf _delimIdent

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
