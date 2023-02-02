-- |
-- Module      : Verismith.Verilog.Internal
-- Description : Defaults and common functions.
-- Copyright   : (c) 2018-2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defaults and common functions.
module Verismith.Verilog.Internal
  ( regDecl,
    wireDecl,
    emptyMod,
    setModName,
    addModPort,
    addModDecl,
    testBench,
    addTestBench,
    defaultPort,
    portToExpr,
    modName,
    yPort,
    wire,
    reg,
  )
where

import Optics ((.~), (%~))
import Data.Text (Text)
import Verismith.Verilog.AST

regDecl :: Identifier -> (ModItem ann)
regDecl i = Decl Nothing (Port Reg False (Range 1 0) i) Nothing

wireDecl :: Identifier -> (ModItem ann)
wireDecl i = Decl Nothing (Port Wire False (Range 1 0) i) Nothing

-- | Create an empty module.
emptyMod :: (ModDecl ann)
emptyMod = ModDecl "" [] [] [] []

-- | Set a module name for a module declaration.
setModName :: Text -> (ModDecl ann) -> (ModDecl ann)
setModName str = #id .~ Identifier str

-- | Add a input port to the module declaration.
addModPort :: Port -> (ModDecl ann) -> (ModDecl ann)
addModPort port = #inPorts %~ (:) port

addModDecl :: (ModDecl ann) -> (Verilog ann) -> (Verilog ann)
addModDecl desc = #_Verilog %~ (:) desc

testBench :: (ModDecl ann)
testBench =
  ModDecl
    "main"
    []
    []
    [ regDecl "a",
      regDecl "b",
      wireDecl "c",
      ModInst
        "and"
        []
        "and_gate"
        [ModConn $ Id "c", ModConn $ Id "a", ModConn $ Id "b"],
      Initial $
        SeqBlock
          [ BlockAssign . Assign (RegId "a") Nothing $ Number 1,
            BlockAssign . Assign (RegId "b") Nothing $ Number 1
          ]
    ]
    []

addTestBench :: (Verilog ann) -> (Verilog ann)
addTestBench = addModDecl testBench

defaultPort :: Identifier -> Port
defaultPort = Port Wire False (Range 1 0)

portToExpr :: Port -> Expr
portToExpr (Port _ _ _ i) = Id i

modName :: (ModDecl ann) -> Text
modName (ModDecl name _ _ _ _) = name.getIdentifier
modName (ModDeclAnn _ m) = modName m

yPort :: Identifier -> Port
yPort = Port Wire False (Range 90 0)

wire :: Range -> Identifier -> Port
wire = Port Wire False

reg :: Range -> Identifier -> Port
reg = Port Reg False
