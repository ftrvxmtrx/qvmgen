-- |Quake VM builtin functions reader.
module Builtin (getBuiltins) where

import Control.Applicative (liftA)
import Control.Arrow

import Data.List (isPrefixOf)
import Data.Map (Map)

import Language.C
import Language.C.Analysis
import Language.C.Data.Ident (identToString)
import Language.C.System.GCC (newGCC)

-- |Alias to a map of functions declarations.
type GObjs = Map Ident IdentDecl

-- |Checks if the declaration is a Quake VM builtin.
isBuiltin :: FilePath -> DeclEvent -> Bool
isBuiltin input (DeclEvent (FunctionDef f)) =
  Just input == fileOfNode f && prefixed
  where prefixed = ("vm_b_" `isPrefixOf`) . identToString . declIdent $ f
isBuiltin _ _ =
  False

-- |Returns a map of builtins from a source file.
getBuiltins :: [String] -> FilePath -> IO GObjs
getBuiltins cflags input =
  liftA getBuiltins
  (parseCFile (newGCC "gcc") Nothing cflags input -- parse
   >>= step "parse" >>=
   (runTrav_ >>> step "analysis") . analyseAST)   -- analyse
  where
    -- |Raise an error or continue.
    step :: (Show a) => String -> (Either a b) -> IO b
    step label = either (error . (concat ["[", label, "] "] ++) . show) return
    -- |Get builtins map from globals.
    getBuiltins :: (GlobalDecls, b) -> GObjs
    getBuiltins = gObjs . filterGlobalDecls (isBuiltin input) . fst
