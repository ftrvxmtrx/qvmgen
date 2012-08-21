-- |Quake VM builtin functions reader.
module Builtin (getBuiltins) where

import Control.Arrow ((>>>))

import Data.List (isPrefixOf)
import Data.Map (toList)

import Language.C
import Language.C.Analysis
import Language.C.System.GCC (newGCC)

-- |Alias to a map of functions declarations.
type GObj = (Ident, FunDef)

-- |Checks if the declaration is a Quake VM builtin.
isBuiltin :: FilePath -> DeclEvent -> Bool
isBuiltin input (DeclEvent (FunctionDef f)) =
  Just input == fileOfNode f && prefixed
  where prefixed = ("vm_b_" `isPrefixOf`) . identToString . declIdent $ f
isBuiltin _ _ =
  False

-- |Returns a map of builtins from a source file.
getBuiltins :: [String] -> FilePath -> IO [GObj]
getBuiltins cflags source = do
  ast <- parseCFile (newGCC "gcc") Nothing cflags source >>= step "parse"
  (globals, _warnings) <- (runTrav_ >>> step "analyse") $ analyseAST ast
  return $ getBuiltins globals
  where
    -- |Raise an error or continue.
    step :: (Show a) => String -> (Either a b) -> IO b
    step label = either (error . (concat ["[", label, "] "] ++) . show) return
    -- |Get builtins map from globals.
    getBuiltins :: GlobalDecls -> [GObj]
    getBuiltins = toList . funDefs . gObjs . filterGlobalDecls (isBuiltin source)
    funDefs objs =
      let (_, (_, _, funDefMap)) = splitIdentDecls False objs in
      funDefMap
