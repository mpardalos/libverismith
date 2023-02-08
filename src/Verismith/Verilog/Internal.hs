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

regDecl :: Annotation ann => Identifier -> ModItem ann
regDecl i = Decl def Nothing (Port Reg False (Range 1 0) i) Nothing

wireDecl :: Annotation ann => Identifier -> ModItem ann
wireDecl i = Decl def Nothing (Port Wire False (Range 1 0) i) Nothing

-- | Create an empty module.
emptyMod :: Annotation ann => ModDecl ann
emptyMod = ModDecl def "" [] [] [] []

-- | Set a module name for a module declaration.
setModName :: Text -> ModDecl ann -> ModDecl ann
setModName str = #id .~ Identifier str

-- | Add a input port to the module declaration.
addModPort :: Port ann -> ModDecl ann -> ModDecl ann
addModPort port = #inPorts %~ (:) port

addModDecl :: ModDecl ann -> Verilog ann -> Verilog ann
addModDecl desc = #_Verilog %~ (:) desc

testBench :: Annotation ann => ModDecl ann
testBench =
  ModDecl
    def
    "main"
    []
    []
    [ regDecl "a",
      regDecl "b",
      wireDecl "c",
      ModInst
        def
        "and"
        []
        "and_gate"
        [ModConn $ Id def "c", ModConn $ Id def "a", ModConn $ Id def "b"],
      Initial def $
        SeqBlock def
          [ BlockAssign def (Assign (RegId "a") Nothing 1),
            BlockAssign def (Assign (RegId "b") Nothing 1)
          ]
    ]
    []

addTestBench :: Annotation ann => Verilog ann -> Verilog ann
addTestBench = addModDecl testBench

defaultPort :: Annotation ann => Identifier -> Port ann
defaultPort = Port Wire False (Range 1 0)

portToExpr :: Annotation ann => Port ann -> Expr ann
portToExpr (Port _ _ _ i) = Id def i

modName :: ModDecl ann -> Text
modName (ModDecl _ name _ _ _ _) = name.getIdentifier

yPort :: Annotation ann => Identifier -> Port ann
yPort = Port Wire False (Range 90 0)

wire :: Range ann -> Identifier -> Port ann
wire = Port Wire False

reg :: Range ann -> Identifier -> Port ann
reg = Port Reg False
