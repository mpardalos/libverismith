-- |
-- Module      : Verismith.Internal
-- Description : Shared high level code used in the other modules internally.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Shared high level code used in the other modules internally.
module Verismith.Internal
  ( -- * Useful functions
    safe,
    showT,
    comma,
    commaNL,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l = Just $ f l

-- | Show function for 'Text'
showT :: (Show a) => a -> Text
showT = T.pack . show

-- | Inserts commas between '[Text]' and except the last one.
comma :: [Text] -> Text
comma = T.intercalate ", "

-- | Inserts commas and newlines between '[Text]' and except the last one.
commaNL :: [Text] -> Text
commaNL = T.intercalate ",\n"
