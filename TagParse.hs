-- |Quake VM tags parser.
module TagParse (Tag, TagInterval, TagList, getTags) where

import Control.Arrow
import Control.Monad

import Data.Maybe

import Language.C.Comments (Comment,
                            comments,
                            commentText,
                            commentTextWithoutMarks,
                            commentPosition)
import Language.C.Data.Position (Position,
                                 posRow)

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

-- |Tag.
data Tag = Builtin                -- ^Builtin
           { name     :: String   -- ^Builtin name.
           , hasTest  :: Bool     -- ^True if has test.
           , vmFilter :: VMFilter -- ^Non-empty if VMs where specified explicitely.
           , index    :: Integer  -- ^Index.
           , sig      :: Sig      -- ^Signature.
           }
         | Comment String
         | DefConst               -- ^Definition of a constant.
           { name  :: String      -- ^Name of the constant.
           , value :: String      -- ^Value of the constant.
           }
         | DefField               -- ^Definition of a field.
           { name  :: String      -- ^Name of the field.
           , type' :: BaseType    -- ^Type of the field.`
           }
         | DefConCmd              -- ^Definition of a console command.
           { name :: String       -- ^Name of the command.
           }
         | Extension String VMFilter         -- ^Extension.
         | ExtensionAddition String VMFilter -- ^Extension which adds some functionality to an existing stuff.
         | Fixme String                      -- ^FIXME tag.
         | Separator                         -- ^Logical separation between tags contained in one comment.
         deriving Show

-- Function signature.
data Sig = Arg Type Sig                 -- ^Normal argument.
         | OptArgs OptArgs (Maybe Type) -- ^Optional argument.
         | Return (Maybe Type)          -- ^End of arguments.
         deriving Show

-- |Optional arguments.
data OptArgs = OptArg String Type (Maybe OptArgs) -- ^Optional argument.
             | VarArg0 Type                       -- ^Zero to many optional arguments.
             | VarArg1 Type                       -- ^One to many optional arguments.
             deriving Show

-- |Quake types.
data Type = Value BaseType -- ^Value.
          | Field BaseType -- ^Pointer to a value.
          | AnyValue       -- ^Value of any type.
          | AnyField       -- ^Pointer to a value of any type.
          deriving Show

-- |Base Quake types.
data BaseType = QBool
              | QInt
              | QString
              | QFloat
              | QVector
              | QEntity
              | QFunc
              deriving (Eq, Show)

-- |VM filter. Non-empty if VMs are specified explicitely.
type VMFilter = [String]

-- |Tag interval (start and end positions).
type TagInterval = (Position, Position)

-- |List of tags from one comment.
type TagList = (TagInterval, [Tag])

-- |Get all tags from source file.
getTags :: FilePath -> IO [Either ParseError TagList]
getTags source = do
  liftM (map m) $ comments source
    where
      m :: Comment -> Either ParseError TagList
      m c =
        addIterval <<< parse (updatePos >> tagParser) source . commentTextWithoutMarks $ c
        where
          addIterval = right $ \x -> (getInterval c, x)
          row        = posRow . commentPosition $ c
          updatePos  = getPosition >>= \p -> setPosition $ setSourceLine p row

-- |Get interval of a comment.
getInterval :: Comment -> TagInterval
getInterval =
  commentPosition &&& length . filter ('\n' ==) . commentText >>>
  \(p, n) -> (p, p { posRow = posRow p + n})

-- |Comment parser. It returns either a tag or a comment not attached to any tag.
tagParser =
  try (do { m_whiteSpace
          ; tags <- many1 tag
          ; eof
          ; return tags
          })
  <|>
  do { m_whiteSpace
     ; c <- anyChar `manyTill` eof
     ; return [Comment c]
     }

-- |Tag parser.
tag =
  separator
  <|>
  try (do { m_whiteSpace
          ; builtin <|> definition <|> extension
          })
  <|>
  fixme
  <|>
  comment
  <?> "tag"

-- |Separator makes subtags logically separated.
separator =
  do { newline
     ; return Separator
     }

-- |Comment. Possibly attached to a tag.
comment =
  return Comment `ap` tillNewLine

-- |"FIXME" tag.
fixme =
  return Fixme `ap` (m_reserved "FIXME" >> tillNewLine)

-- |Helper to read everything to newline.
tillNewLine =
  anyChar `manyTill` newline

-- |Constant, console command or a field.
definition =
  do { m_reserved "const"
     ; name <- m_identifier
     ; m_reservedOp "="
     ; value <- tillNewLine
     ; return DefConst{ name  = name
                      , value = value
                      }
     }
  <|>
  do { m_reserved "concmd"
     ; name <- m_stringLiteral
     ; return DefConCmd{ name = name }
     }
  <|>
  do { char '.'
     ; type' <- baseType
     ; name <- m_identifier
     ; return DefField{ name  = name
                      , type' = type'
                      }
     }

-- |Extension
extension =
  do { m_reservedOp "@+" -- this extension adds something to an existing functionality
     ; return ExtensionAddition `ap` m_identifier `ap` vmFilterExpr
     }
  <|>
  do { m_reservedOp "@" -- whole is "attached" to the extension
     ; return Extension `ap` m_identifier `ap` vmFilterExpr
     }
  <?> "extension"

-- |Base types parser.
baseType =
    bt "bool"   QBool   <|>
    bt "entity" QEntity <|>
    bt "float"  QFloat  <|>
    bt "func"   QFunc   <|>
    bt "int"    QInt    <|>
    bt "string" QString <|>
    bt "vector" QVector <?> "base type"
  where
    bt s t = m_reserved s >> return t

-- |VM filter parser.
vmFilterExpr =
  option [] (m_parens $ m_commaSep1 m_identifier)

-- |Builtin function parser.
builtin =
  do { hasTest  <- liftM isJust $ optionMaybe $ char 'T'
     ; index    <- between (m_reservedOp "#") (m_reservedOp "=") m_integer
     ; name     <- m_identifier
     ; vmFilter <- vmFilterExpr
     ; m_reservedOp "::"
     ; sig      <- signature
     ; return Builtin{ index    = index
                     , name     = name
                     , sig      = sig
                     , hasTest  = hasTest
                     , vmFilter = vmFilter
                     }
     }
  where
    signature = (return Return `ap` (m_reserved "()" >> m_reservedOp "->" >> sigRet))
                <|>
                (return     id `ap` sigWithArgs)
                <?> "function signature"

    sigWithArgs = try (do { type' <- anyType
                          ; m_reservedOp "->"
                          ; tail  <- sigWithArgs
                          ; return $ Arg type' tail
                          })
                  <|>
                  try (do { args <- optArgs
                          ; ret  <- sigRet
                          ; return $ OptArgs args ret
                          })
                  <|>
                  do { ret <- sigRet
                     ; return $ Return ret
                     }
                  <?> "function signature with args"

    sigRet = (return (const Nothing) `ap` m_reserved "()")
             <|>
             (return           Just  `ap` anyType)
             <?> "function return type"

    -- OptArgs
    optArgs = optArg <|> varArg0 <|> varArg1 <?> "optional arguments"
    optArg = do { (name, type') <- m_braces optArgNameType
                ; m_reservedOp "->"
                ; tail <- optionMaybe optArg
                ; return $ OptArg name type' tail
                }
    varArg0 = do { m_reservedOp "..."
                 ; type' <- option AnyValue anyType
                 ; m_reservedOp "->"
                 ; return $ VarArg0 type'
                 }
    varArg1 = do { type' <- anyType
                 ; m_reservedOp "..."
                 ; m_reservedOp "->"
                 ; return $ VarArg1 type'
                 }

    optArgNameType = do { name <- m_identifier
                        ; m_reservedOp "::"
                        ; type' <- anyType
                        ; return (name, type')
                        }

    -- Type
    typeValue    = return           Value  `ap` baseType
    typeAnyValue = return (const AnyValue) `ap` m_reserved "any"
    typeField    = return           Field  `ap` (char '.' >> baseType)
    typeAnyField = return (const AnyField) `ap` (char '.' >> m_reserved "any")

    anyType = try typeField <|> try typeAnyField <|> try typeValue <|> typeAnyValue <?> "type"

-- |Main token parser for Parsec.
TokenParser{ identifier    = m_identifier
           , reservedOp    = m_reservedOp
           , reserved      = m_reserved
           , integer       = m_integer
           , whiteSpace    = m_whiteSpace
           , braces        = m_braces
           , parens        = m_parens
           , commaSep1     = m_commaSep1
           , stringLiteral = m_stringLiteral
           } = makeTokenParser def
  where
    def =
      emptyDef{ commentStart    = "{-"
              , commentEnd      = "-}"
              , identStart      = letter <|> char '_'
              , identLetter     = alphaNum <|> char '_'
              , reservedOpNames = ["->", "::", "#", "=", "...", "@", "@+"]
              , reservedNames   = [ "()"
                                  , "any"
                                  , "bool"
                                  , "concmd"
                                  , "const"
                                  , "entity"
                                  , "FIXME"
                                  , "float"
                                  , "func"
                                  , "int"
                                  , "string"
                                  , "vector"
                                  ]
              }
