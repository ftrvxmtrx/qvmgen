-- |Quake VM tags parser.
module TagParse (Tag(..),
                 TagData(..),
                 Sig(..),
                 BaseType(..),
                 Type(..),
                 OptArgs(..),
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
  | Constant{ vmFilter :: VMFilter
            , name     :: String     -- ^Name of the constant.
            , value    :: ConstValue -- ^Value of the constant.
            }
  -- |Declaration.
  | Declaration{ name     :: String   -- ^Name of the declaration.
               , vmFilter :: VMFilter -- ^Non-empty if should exist only in specific VMs.
               , type'    :: Type     -- ^Type of the declaration.
               }
  -- |Console alias.
  | ConsoleAlias{ vmFilter :: VMFilter
                , name     :: String
                }
  -- |Console command.
  | ConsoleCommand{ vmFilter :: VMFilter
                  , name     :: String
                  }
  -- |Console variable.
  | ConsoleVariable{ vmFilter :: VMFilter
                   , name     :: String
                   }
  -- |Extension.
  | Extension{ name     :: String
             , vmFilter :: VMFilter
             }
  -- |Extension which adds some functionality to an existing stuff.
  | ExtensionAddition{ name     :: String
                     , vmFilter :: VMFilter
                     }
  -- |FIXME tag.
  | Fixme String
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

-- |Parser state.
data TagState = TagState{ lastVMFilter :: VMFilter
                        }

-- |Initial parser state.
initialState =
  TagState{ lastVMFilter = []
          }

-- |Get all tags from source file.
getTags :: [String] -> FilePath -> IO [Either ParseError Tag]
getTags cflags source = do
  builtinDefs <- getBuiltins cflags source
  liftM (map $ m builtinDefs) $ comments source
    where
      m :: BuiltinDefs -> Comment -> Either ParseError Tag
      m builtinDefs c = do
        bDef <- return $ M.lookup (posRow endPos) builtinDefs
        tagData <- runParser (updatePos >> tagParser bDef) initialState source . commentTextWithoutMarks $ c
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
  (optional m_whiteSpace >> tag bDef) `manyTill` eof

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
  return Comment `ap` ((m_reservedOp "//" >> anyChar `manyTill` newline)
                       <|>
                       (anyChar `manyTill` (try (newline >> skipMany1 newline)
                                            <|>
                                            eof)))

-- |"FIXME" tag.
fixme =
  return Fixme `ap` (m_reserved "FIXME" >> many1 (noneOf "\n"))

-- |Constant.
constant =
  return Constant
  `ap` (m_reserved "const" >> vmFilterExpr)
  `ap` m_identifier
  `ap` (m_reservedOp "=" >> constValue)
  where
    constValue =
      choice [ return ConstFloat   `ap` try m_float
             , return ConstInteger `ap` m_integer
             , return ConstString  `ap` m_stringLiteral
             ]

-- |Console object.
console =
  choice [ conObj ConsoleAlias    "conalias"
         , conObj ConsoleCommand  "concmd"
         , conObj ConsoleVariable "convar"
         ]
  where conObj t s =
          return t `ap` (m_reserved s >> vmFilterExpr) `ap` m_stringLiteral

-- |Declaration.
declaration =
  try $ namedTypedDeclaration True

-- |Name and type (name :: type).
namedTypedDeclaration withVMFilter =
  do { name <- m_identifier
     ; vmFilter <- if withVMFilter then vmFilterExpr else return []
     ; m_reservedOp "::"
     ; type' <- anyType
     ; return Declaration{ name     = name
                         , vmFilter = vmFilter
                         , type'    = type'
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

-- |VM filter parser.
vmFilterExpr =
  do { vmFilter <- m_parens $ m_commaSep1 m_identifier
     ; modifyState (\s@TagState{} -> s{ lastVMFilter = vmFilter })
     ; return vmFilter
     }
  <|>
  do { st <- getState
     ; return $ lastVMFilter st
     }

-- |Builtin function parser.
builtin bDef =
  do { (hasTest, index) <- try testAndIndex
     ; name             <- m_identifier
     ; vmFilter         <- vmFilterExpr
     ; m_reservedOp "::"
     ; rest hasTest index name vmFilter bDef
     }
  where
    rest hasTest index name vmFilter (Just f@(A.FunDef _ _ _)) =
      do { sig <- signature <?> "signature"
         ; return Builtin{ index    = index
                         , cFunc    = identToString . A.declIdent $ f
                         , name     = name
                         , sig      = sig
                         , hasTest  = hasTest
                         , vmFilter = vmFilter
                         }
         }
    rest _ _ _ _ Nothing =
      unexpected "tag with no builtin definition"
    testAndIndex = do { hasTest <- liftM isJust $ optionMaybe $ char 'T'
                      ; index   <- between (m_reservedOp "#") (m_reservedOp "=") m_integer
                      ; return (hasTest, index)
                      }

-- |Function signature parser.
signature =
  (return Return `ap` (m_reserved "()" >> m_reservedOp "->" >> sigRet))
  <|>
  (return     id `ap` sigWithArgs)

  where
    sigWithArgs = try (do { decl <- m_parens $ namedTypedDeclaration False
                          ; m_reservedOp "->"
                          ; tail <- sigWithArgs
                          ; return $ Arg (name decl) (type' decl) tail
                          })
                  <|>
                  try (do { type' <- anyType
                          ; m_reservedOp "->"
                          ; tail  <- sigWithArgs
                          ; return $ Arg "" type' tail
                          })
                  <|>
                  try (return OptArgs `ap` optArgs `ap` sigRet)
                  <|>
                  return Return `ap` sigRet
                  <?> "function signature with args"

    sigRet = (return (const Nothing) `ap` m_reserved "()")
             <|>
             (return           Just  `ap` anyType)
             <?> "function return type"

    -- OptArgs
    optArgs = optArg <|> varArg0 <|> varArg1 <?> "optional arguments"
    optArg = do { decl <- m_braces $ namedTypedDeclaration False
                ; m_reservedOp "->"
                ; tail <- optionMaybe optArg
                ; return $ OptArg (name decl) (type' decl) tail
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
