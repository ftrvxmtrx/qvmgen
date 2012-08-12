-- |Quake VM specific tags.
module Tag (Tag, getTags) where

import Control.Arrow

import Language.C.Comments (Comment,
                            comments,
                            commentText,
                            commentPosition)
import Language.C.Data.Position (Position,
                                 posRow)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

-- |Quake VM specific tag.
data Tag = Tag ([TagData], TagInterval)
         deriving Show

-- |Tag interval (start and end positions).
type TagInterval = (Position, Position)

-- |Builtin function definition.
data BuiltinDefinition = BuiltinDefinition
                         { name  :: String,           -- ^Builtin name.
                           index :: Int,              -- ^Index.
                           sig   :: BuiltinSignature, -- ^Signature.
                           exts  :: [QExtensionRef]   -- ^Extensions list.
                         } deriving Show

-- |Builtin function signature.
data BuiltinSignature = BuiltinSignature (Maybe QType, Maybe BuiltinArgs)
                      deriving Show

-- |Base Quake VM types.
data QBaseType = QBool
               | QInt
               | QString
               | QFloat
               | QVector
               | QEntity
               | QFunction
               deriving (Eq, Show)

-- |Quake VM types.
data QType = QValue QBaseType -- ^Plain value.
           | QField QBaseType -- ^Field of a specific type.
           deriving Show

-- |Argument.
type QArg = (String, QType)

-- |Arguments of builtin function.
data BuiltinArgs = Arg (QArg, Maybe BuiltinArgs)
                 | OptionalArgs [QArg]
                 deriving Show

-- |Tag data.
data TagData = TBuiltin (BuiltinDefinition, [Description])
             deriving Show

-- |Description of tag.
type Description = String

-- |Reference to an extension by its name.
type QExtensionRef = String

-- |Get interval of a comment.
getInterval :: Comment -> TagInterval
getInterval =
  commentPosition &&& length . filter ('\n' ==) . commentText >>>
  \(p, n) -> (p, p { posRow = posRow p + n})

-- |Get all tags from source file.
getTags :: FilePath -> IO [Tag]
getTags source = do
  cs <- comments source
  return []
