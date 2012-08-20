-- |Quake VM data reader.
module Read (readFrom) where

import Builtin

import Data.List (sortBy)

import Language.C.Data.Position (posOf, posRow)

import TagParse

-- |Reads Quake VM data from C source.
readFrom :: [String] -> FilePath -> IO ()
readFrom cflags source = do
  tags <- getTags source
  builtins <- getBuiltins cflags source
  mapM_ print $ sort [ (name, snd t) | (name, b) <- builtins
                                     , Right t <- tags
                                     , rowOfBuiltin b == rowOfTagEnd t
                                     ]
  where
    rowOfBuiltin = posRow . posOf
    rowOfTagEnd = posRow . snd . fst
    sort = sortBy $ \(_, Builtin{index = x}:_) (_, Builtin{index = y}:_) -> compare x y
