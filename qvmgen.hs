-- |qvmgen generates Quake VM code and documentation based on builtins, extensions, etc.
module Main (main) where

import Control.Applicative (liftA)
import Control.Arrow
import Control.Monad (forM_)
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Language.C
import Language.C.Analysis
import Language.C.Comments (comments)
import Language.C.System.GCC (newGCC)
import System.Console.GetOpt
import System.Environment (getArgs)

-- |Command line options.
data Flag = Verbose
          | Input FilePath
          | Output FilePath
          deriving (Eq, Show)

-- |Alias to a map of functions declarations.
type GObjs = Map Ident IdentDecl

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose)          "print additional information while processing sources"
  , Option ['i'] ["input"]   (ReqArg Input "INPUT")   "input source filename"
  , Option ['o'] ["output"]  (ReqArg Output "OUTPUT") "output source filename"
  ]

-- |qvmgen options parser.
qvmgenOpts :: [String] -> IO ([Flag], [String])
qvmgenOpts argv =
  case getOpt RequireOrder options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo header options)
      where header = "Usage: qvmgen [OPTION...] CFLAGS..."

-- |Checks if the declaration is a Quake VM builtin.
isBuiltin :: FilePath -> DeclEvent -> Bool
isBuiltin input (DeclEvent (FunctionDef f)) =
  Just input == fileOfNode f
isBuiltin _ _ =
  False

-- |Returns a map of builtins from a source file.
getBuiltins :: [String] -> FilePath -> IO GObjs
getBuiltins cflags input =
  liftA getBuiltins
  (parseCFile (newGCC "gcc") Nothing cflags input >>= -- parse
   step "parse" >>=
   (runTrav_ >>> step "analysis") . analyseAST)       -- analyse
  where
    -- |Raise an error or continue.
    step :: (Show a) => String -> (Either a b) -> IO b
    step label = either (error . (("[" ++ label ++ "] ") ++) . show) return
    -- |Get builtins map from globals.
    getBuiltins :: (GlobalDecls, b) -> GObjs
    getBuiltins = gObjs . filterGlobalDecls (isBuiltin input) . fst

-- |Generates Quake VM source code.
gen :: [String] -> FilePath -> FilePath -> IO ()
gen cflags input output = do
  cs <- comments input
  getBuiltins cflags input >>= print . map fst . toList

main = do
  (args, cflags) <- getArgs >>= qvmgenOpts
  let verbose = Verbose `elem` args
  zip [x | Input x <- args] [x | Output x <- args] `forM_` (uncurry $ gen cflags)
