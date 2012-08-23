-- |Quake VM data reader.
module Read (readFrom) where

import TagParse

-- |Reads Quake VM data from C source.
readFrom :: [String] -> FilePath -> IO ()
readFrom cflags source = do
  tags <- getTags cflags source
  print tags
  -- builtins <- getBuiltins cflags source
  -- mapM_ print $ sort [ (pretty . declType $ funDef, tagData t) | (row, funDef) <- toList builtins
  --                                                              , Right t <- tags
  --                                                              , row == (posRow . endPos $ t)
  --                                                              ]
  -- where
  --   sort = sortBy $ \(_, Builtin{index = x}:_) (_, Builtin{index = y}:_) -> compare x y
