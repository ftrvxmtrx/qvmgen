-- |Quake VM data reader.
module Read (readFrom) where

import Builtin

import Data.Map (Map, toList)

import Tag

-- |Reads Quake VM data from C source.
readFrom :: [String] -> FilePath -> IO ()
readFrom cflags source = do
  getTags source
  getBuiltins cflags source >>= print . map fst . toList
