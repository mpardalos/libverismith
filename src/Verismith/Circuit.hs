-- |
-- Module      : Verismith.Circuit
-- Description : Definition of the circuit graph.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definition of the circuit graph.
module Verismith.Circuit
  ( -- * Circuit
    Gate (..),
    Circuit (..),
    CNode (..),
    CEdge (..),
    rDups,
    rDupsCirc,
    randomDAG,
    genRandomDAG,
  )
where

import Control.Lens
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Hog
import Verismith.Circuit.Base
import Verismith.Circuit.Random
import Verismith.Verilog.AST
import Verismith.Verilog.Mutate
