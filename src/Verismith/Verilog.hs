{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Verismith.Verilog
-- Description : Verilog implementation with random generation and mutations.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Verilog implementation with random generation and mutations.
module Verismith.Verilog
  ( SourceInfo (..),
    Verilog (..),
    parseVerilog,
    GenVerilog (..),
    genSource,

    -- * Primitives

    -- ** Identifier
    Identifier (..),

    -- ** Control
    Delay (..),
    Event (..),

    -- ** Operators
    BinaryOperator (..),
    UnaryOperator (..),

    -- ** Task
    Task (..),

    -- ** Left hand side value
    LVal (..),

    -- ** Ports
    PortDir (..),
    PortType (..),
    Port (..),

    -- * Expression
    Expr (..),
    ConstExpr (..),
    constToExpr,
    exprToConst,

    -- * Assignment
    Assign (..),
    ContAssign (..),

    -- * Statment
    Statement (..),

    -- * Module
    ModDecl (..),
    ModItem (..),
    traverseModItem,
    ModConn (..),

    -- * Useful Lenses and Traversals
    getModule,
    getSourceId,

    -- * Quote
    verilog,
  )
where

import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Parser
import Verismith.Verilog.Quote
