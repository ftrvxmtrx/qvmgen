-- |Quake VM builtin functions reader.
module Builtin (BuiltinDefs, getBuiltins) where

import Control.Arrow ((>>>))

import Data.List (isPrefixOf)
import Data.Map (Map, fromList, toList)

import Language.C
import Language.C.Analysis
import Language.C.System.GCC (newGCC)

-- |Alias to a map of functions declarations.
type BuiltinDefs = Map Int FunDef

-- |Checks if the declaration is a Quake VM builtin.
isBuiltin :: FilePath -> DeclEvent -> Bool
isBuiltin input (DeclEvent (FunctionDef f)) =
  Just input == fileOfNode f && prefixed
  where prefixed = ("vm_b_" `isPrefixOf`) . identToString . declIdent $ f
isBuiltin _ _ =
  False

-- |Returns a map of builtins from a source file.
getBuiltins :: [String] -> FilePath -> IO BuiltinDefs
getBuiltins cflags source = do
  ast <- parseCFile (newGCC "gcc") Nothing cflags source >>= step "parse"
  (globals, _warnings) <- (runTrav_ >>> step "analyse") $ analyseAST ast
  return $ getBuiltins globals
  where
    -- |Raise an error or continue.
    step :: (Show a) => String -> (Either a b) -> IO b
    step label = either (error . (concat ["[", label, "] "] ++) . show) return
    -- |Get builtins map from globals.
    getBuiltins :: GlobalDecls -> BuiltinDefs
    getBuiltins = remap . funDefs . gObjs . filterGlobalDecls (isBuiltin source)
    funDefs objs =
      let (_, (_, _, funDefMap)) = splitIdentDecls False objs in
      funDefMap
    remap :: Map Ident FunDef -> BuiltinDefs
    remap  =
      fromList . map (\(_ident, funDef) -> (posRow . posOf $ funDef, funDef)) . toList
