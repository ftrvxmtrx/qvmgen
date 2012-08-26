-- |Quake VM tags parser.
module TagParse (Tag(..),
                 TagData(..),
                 getTags) where

import Builtin

import Control.Arrow
import Control.Monad

import Data.Maybe
import qualified Data.Map as M

import qualified Language.C.Analysis as A
import Language.C.Comments (Comment,
                            comments,
                            commentText,
                            commentTextWithoutMarks,
                            commentPosition)
import Language.C.Data.Ident (identToString)
import Language.C.Data.Position (Position,
                                 posRow)

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

-- |Tag data from one comment.
data Tag = Tag{ startPos :: Position
              , endPos   :: Position
              , tagData  :: [TagData]
              }
           deriving Show

-- |Tag data.
data TagData =
  -- |Builtin
  Builtin{ name     :: String   -- ^Builtin name.
         , cFunc    :: String   -- ^C function name.
         , hasTest  :: Bool     -- ^True if has test.
         , vmFilter :: VMFilter -- ^Non-empty if VMs where specified explicitely.
         , index    :: Integer  -- ^Index.
         , sig      :: Sig      -- ^Signature.
         }
  -- | Comment.
  | Comment String
  -- |Definition of a constant.
  | Constant{ name  :: String     -- ^Name of the constant.
            , value :: ConstValue -- ^Value of the constant.
            }
  -- |Declaration.
  | Declaration{ name  :: String -- ^Name of the declaration.
               , type' :: Type   -- ^Type of the declaration.
               }
  -- |Console command.
  | ConsoleCommand String
  -- |Console variable.
  | ConsoleVariable String
  -- |Console alias.
  | ConsoleAlias String
  -- |Extension.
  | Extension String VMFilter
  -- |Extension which adds some functionality to an existing stuff.
  | ExtensionAddition String VMFilter
  -- |FIXME tag.
  | Fixme String
  -- |Parse error.
  | TagError{ relatedData :: TagData -- ^Data related to the error.
            , errorMsg    :: String  -- ^Error message.
            }
  deriving Show

-- |Constant value.
data ConstValue = ConstString String
                | ConstInteger Integer
                | ConstFloat Double
                deriving Show

-- |Function signature.
data Sig = Arg ArgName Type Sig         -- ^Normal argument.
         | OptArgs OptArgs (Maybe Type) -- ^Optional argument.
         | Return (Maybe Type)          -- ^End of arguments.
         deriving Show

-- |Optional arguments.
data OptArgs = OptArg ArgName Type (Maybe OptArgs) -- ^Optional argument.
             | VarArg0 ArgName ArgStartIndex Type  -- ^Zero to many optional arguments.
             | VarArg1 ArgName ArgStartIndex Type  -- ^One to many optional arguments.
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
              | QFunc Sig
              deriving Show

type ArgName = String
type ArgStartIndex = Integer

-- |VM filter. Non-empty if VMs are specified explicitely.
type VMFilter = [String]

-- |Get all tags from source file.
getTags :: [String] -> FilePath -> IO [Either ParseError Tag]
getTags cflags source = do
  builtinDefs <- getBuiltins cflags source
  liftM (map $ m builtinDefs) $ comments source
    where
      m :: BuiltinDefs -> Comment -> Either ParseError Tag
      m builtinDefs c = do
        bDef <- return $ M.lookup (posRow endPos) builtinDefs
        tagData <- parse (updatePos >> tagParser bDef) source . commentTextWithoutMarks $ c
        return Tag{ startPos = startPos
                  , endPos   = endPos
                  , tagData  = tagData
                  }
        where
          row        = posRow . commentPosition $ c
          updatePos  = getPosition >>= \p -> setPosition $ setSourceLine p row
          (startPos, endPos) = getInterval c

-- |Get interval of a comment.
getInterval :: Comment -> (Position, Position)
getInterval =
  commentPosition &&& length . filter ('\n' ==) . commentText >>>
  \(p, n) -> (p, p { posRow = posRow p + n + 1})

-- |Comment parser. It returns either a tag or a comment not attached to any tag.
tagParser bDef =
  do { tags <- (do { optional m_whiteSpace
                   ; tag bDef
                   }) `manyTill` eof
     ; return tags
     }

-- |Tag parser.
tag bDef =
  choice [ fixme
         , builtin bDef
         , console
         , constant
         , declaration
         , extension
         , comment
         ]
  <?> "tag"

-- |Comment. Possibly attached to a tag.
comment =
  do { m_reservedOp "//"
     ; c <- anyChar `manyTill` newline
     ; return $ Comment c
     }
  <|>
  do { c <- anyChar `manyTill` (try $ do { newline; newline } <|> do { eof; return '\n' })
     ; return $ Comment c
     }

-- |"FIXME" tag.
fixme =
  return Fixme `ap` (m_reserved "FIXME" >> many1 (noneOf "\n"))

-- |Constant.
constant =
  do { m_reserved "const"
     ; name <- m_identifier
     ; m_reservedOp "="
     ; value <- constValue
     ; return Constant{ name  = name
                      , value = value
                      }
     }

-- |Constant value.
constValue =
  choice [ return ConstFloat   `ap` try m_float
         , return ConstInteger `ap` m_integer
         , return ConstString  `ap` m_stringLiteral
         ]

-- |Console object.
console =
  choice [ return ConsoleAlias    `ap` (m_reserved "conalias" >> m_stringLiteral)
         , return ConsoleVariable `ap` (m_reserved "convar"   >> m_stringLiteral)
         , return ConsoleCommand  `ap` (m_reserved "concmd"   >> m_stringLiteral)
         ]

-- |Declaration.
declaration =
  do { (name, type') <- try namedTypedDeclaration
     ; return Declaration{ name  = name
                         , type' = type'
                         }
     }

-- |Name and type (name :: type).
namedTypedDeclaration =
  do { name <- m_identifier
     ; m_reservedOp "::"
     ; type' <- anyType
     ; return (name, type')
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
  choice [ bt "bool"   QBool
         , bt "entity" QEntity
         , bt "float"  QFloat
         , bt "int"    QInt
         , bt "string" QString
         , bt "vector" QVector
         , return QFunc `ap` m_parens signature
         ]
  <?> "base type"
  where
    bt s t = m_reserved s >> return t

-- |VM filter parser.
vmFilterExpr =
  option [] (m_parens $ m_commaSep1 m_identifier)

-- |Builtin function parser.
builtin bDef =
  do { (hasTest, index) <- try testAndIndex
     ; name <- m_identifier
     ; vmFilter <- vmFilterExpr
     ; m_reservedOp "::"
     ; sig <- signature
     ; let t = Builtin{ index    = index
                      , cFunc    = "<invalid>"
                      , name     = name
                      , sig      = sig
                      , hasTest  = hasTest
                      , vmFilter = vmFilter
                      } in
       case bDef of
         (Just f@(A.FunDef _ _ _)) -> return t{ cFunc = identToString . A.declIdent $ f }
         _                         -> return TagError{ relatedData = t
                                                     , errorMsg    = "no builtin definition after tag"
                                                     }
     }
  where
    testAndIndex = do { hasTest <- liftM isJust $ optionMaybe $ char 'T'
                      ; index   <- between (m_reservedOp "#") (m_reservedOp "=") m_integer
                      ; return (hasTest, index)
                      }

-- |Function signature parser.
signature =
  (return Return `ap` (m_reserved "()" >> m_reservedOp "->" >> sigRet))
  <|>
  (return     id `ap` sigWithArgs)
  <?> "function signature"

  where
    sigWithArgs = try (do { (name, type') <- m_parens namedTypedDeclaration
                          ; m_reservedOp "->"
                          ; tail          <- sigWithArgs
                          ; return $ Arg name type' tail
                          })
                  <|>
                  try (do { type' <- anyType
                          ; m_reservedOp "->"
                          ; tail  <- sigWithArgs
                          ; return $ Arg "" type' tail
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
    optArg = do { (name, type') <- m_braces namedTypedDeclaration
                ; m_reservedOp "->"
                ; tail <- optionMaybe optArg
                ; return $ OptArg name type' tail
                }
    varArg0 = do { m_reservedOp "..."
                 ; type' <- option AnyValue anyType
                 ; m_reservedOp "->"
                 ; return $ VarArg0 "" 0 type'
                 }
    varArg1 = do { type' <- anyType
                 ; m_reservedOp "..."
                 ; m_reservedOp "->"
                 ; return $ VarArg1 "" 1 type'
                 }

-- |Type.
anyType =
  choice [ try typeField
         , try typeAnyField
         , try typeValue
         , typeAnyValue
         ]
  <?> "type"
  where
    typeValue    = return           Value  `ap` baseType
    typeAnyValue = return (const AnyValue) `ap` m_reserved "any"
    typeField    = return           Field  `ap` (char '.' >> baseType)
    typeAnyField = return (const AnyField) `ap` (char '.' >> m_reserved "any")

-- |Main token parser for Parsec.
TokenParser{ identifier    = m_identifier
           , reservedOp    = m_reservedOp
           , reserved      = m_reserved
           , integer       = m_integer
           , float         = m_float
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
              , reservedOpNames = ["->", "::", "#", "=", "...", "@", "@+", "//"]
              , reservedNames   = [ "()"
                                  , "any"
                                  , "bool"
                                  , "conalias"
                                  , "concmd"
                                  , "const"
                                  , "convar"
                                  , "entity"
                                  , "FIXME"
                                  , "float"
                                  , "func"
                                  , "int"
                                  , "string"
                                  , "vector"
                                  ]
              }
