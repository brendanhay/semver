-- |
-- Module      : Data.SemVer.QQ
-- Copyright   : (c) 2022 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Quasi-quotation for defining 'Version' constants.
module Data.SemVer.QQ
  ( version
  )
where

import qualified Data.SemVer as Base
import qualified Data.Text as Text
import Control.Monad ((<=<))
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | Build a 'Version'.
--
-- >>> import Data.SemVer (toText)
-- >>> import qualified Data.SemVer.QQ as Q
-- >>> toText [Q.version| 0.1.23-alpha |]
-- "0.1.23-alpha"
--
-- >>> toText [Q.version| 0.1.a |]
-- • Invalid version format
-- • In the quasi-quotation: [Q.version|0.1.a|]
version :: QuasiQuoter
version = QuasiQuoter
  { quoteExp = lift <=< parseVersion
  , quotePat = dataToPatQ (const Nothing) <=< parseVersion
  , quoteType = \_ -> fail "Cannot be used as a type"
  , quoteDec = \_ -> fail "Cannot be used as a declaration"
  }
  where
    parseVersion str =
      case Base.fromText (Text.strip $ Text.pack str) of
        Right x -> pure x
        Left _ -> fail "Invalid version format"
