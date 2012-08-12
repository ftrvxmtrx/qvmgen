-- |qvmgen generates Quake VM code and documentation based on builtins, extensions, etc.
module Main (main) where

import Control.Monad (forM_)

import Read

import qualified System.Console.GetOpt as G
import System.Environment (getArgs)

-- |Command line options.
data Flag = Input FilePath
          | Output FilePath
          deriving Eq

options :: [G.OptDescr Flag]
options =
  [ G.Option ['i'] ["input"]   (G.ReqArg Input "INPUT")   "input source filename"
  , G.Option ['o'] ["output"]  (G.ReqArg Output "OUTPUT") "output source filename"
  ]

-- |qvmgen options parser.
qvmgenOpts :: [String] -> IO ([Flag], [String])
qvmgenOpts argv =
  case G.getOpt G.RequireOrder options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError $ concat errs ++ G.usageInfo header options)
      where header = "Usage: qvmgen [OPTION...] -- CFLAGS..."

main = do
  (args, cflags) <- getArgs >>= qvmgenOpts
  zip [x | Input x <- args] [x | Output x <- args] `forM_` \(source, _) -> readFrom cflags source
