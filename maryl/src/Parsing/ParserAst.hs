{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParserAst (
    list,
    pTerm,
    pAst,
    pIf,
    pElse,
    pReturn,
    operatorTable,
    binary,
    prefix,
    postfix,
    charLiteral,
    stringLiteral,
    integer,
    double,
    pKeyword,
    variable,
    sc,
    scn,
    lexeme,
    symbol,
    convertValue,
    parseAst,
    ternary,
    Ast (..)
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (
    Operator (..),
    makeExprParser,
 )
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (eof, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    many,
    manyTill,
    parse,
    some,
    (<?>),
    (<|>), optional, sepBy, noneOf,
 )
import Text.Megaparsec.Char (char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

data MarylType = String | Integer | Double | Char | Bool | Void deriving (Eq, Ord, Show)

data Function = Function
    { name :: String,
      args :: [Ast]
    } deriving (Eq, Ord, Show)

data Variable = Variable
    { vName :: String,
      vType :: MarylType,
      vValue :: Ast
    } deriving (Eq, Ord, Show)

data Ast
    = AstVar String
    | AstVoid
    | AstInt Integer
    | AstBool Bool
    | AstString String
    | AstChar Char
    | AstDouble Double
    | AstBinaryFunc String Ast Ast
    | AstPostfixFunc String Ast
    | AstPrefixFunc String Ast
    | AstFunc Function
    | AstIf Ast Ast [Ast] (Maybe Ast) -- if cond do [else if] (Maybe else)
    | AstTernary Ast Ast Ast -- cond ? do : else
    | AstReturn Ast
    | AstBlock [Ast]
    | AstLoop Ast Ast -- cond AstBlock
    | AstDefineTyped Variable
    deriving (Eq, Ord, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t' <|> char '\n')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

bonusChar' :: String
bonusChar' = "+-<>*?!=&|^%/~_#$;:"

bonusChar :: Parser Char
bonusChar = choice $ char <$> bonusChar'

variable :: Parser String
variable =
    (:) <$> (try letterChar <|> bonusChar) <*> many (noneOf (" \t\n\r(),=" :: [Char]))
        <?> "variable"

integer :: Parser Integer
integer = lexeme L.decimal

double :: Parser Double
double = lexeme L.float

bool :: Parser Bool
bool =
    lexeme $
        choice
            [ False <$ string "false",
              True <$ string "true"
            ]

pKeyword :: String -> Parser String
pKeyword keyword = lexeme (string keyword)

convertValue :: Parser Ast
convertValue =
    choice
        [ AstDouble <$> try double,
          AstInt <$> integer,
          AstBool <$> bool,
          AstChar <$> charLiteral,
          AstString <$> stringLiteral,
          AstReturn <$> pReturn,
          AstBlock <$> block,
          AstVar <$> variable
        ]

list :: Parser Ast -> Parser Ast
list = between (symbol "(") (symbol ")")

listVariables :: Parser [Ast]
listVariables = between (symbol "(") (symbol ")") (pAst `sepBy` lexeme ",")

block :: Parser [Ast]
block = between (symbol "{") (symbol "}") (many pAst)

types :: Parser String
types = choice
    [ "int"
    , "float"
    , "string"
    , "char"
    , "bool"
    , "void"
    ]

getType :: String -> MarylType
getType "int" = Integer
getType "float" = Double
getType "string" = String
getType "char" = Char
getType "bool" = Bool
getType _ = Void

optionalValue :: Parser (Maybe Ast)
optionalValue = optional $ do
    sc
    _ <- string "="
    sc
    pAst

pDeclaration :: Parser Ast
pDeclaration = do
    t <- types
    sc
    n <- variable
    v <- optionalValue
    return $ AstDefineTyped (Variable {vName=n, vType=getType t, vValue=fromMaybe AstVoid v})

pFunc :: Parser Ast
pFunc = do
    n <- variable
    a <- listVariables
    return $ AstFunc (Function {name=n, args=a})

pLoop :: Parser Ast
pLoop = do
    string "while" >> sc
    cond <- list pAst
    toDo <- AstBlock <$> block
    return $ AstLoop cond toDo

pReturn :: Parser Ast
pReturn = string "return" >> sc >> pAst

pElse :: Parser (Maybe Ast)
pElse = optional $ string "else" >> sc >> AstBlock <$> block >>= \b -> return b

pElseIf :: Parser Ast
pElseIf = try $ do
  string "else if" >> sc
  cond <- list pAst
  toDo <- AstBlock <$> block
  return $ AstIf cond toDo [] Nothing

pIf :: Parser Ast
pIf = do
  string "if" >> sc
  cond <- list pAst
  toDo <- AstBlock <$> block
  elseIf <- many pElseIf
  AstIf cond toDo elseIf <$> pElse

pTerm :: Parser Ast
pTerm =
    choice
        [ try pIf,
          try pLoop,
          try pFunc,
          try pDeclaration,
          list pAst,
          convertValue
        ]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL (f <$ symbol n)

prefix, postfix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix (f <$ symbol n)
postfix n f = Postfix (f <$ symbol n)

ternary :: (a -> a -> a -> a) -> Operator Parser a
ternary f = TernR ((f <$ lexeme (char ':')) <$ lexeme (char '?'))

operatorTable :: [[Operator Parser Ast]]
operatorTable =
    [   [ prefix "--" (AstPrefixFunc "--"),
          prefix "-" (AstPrefixFunc "-"),
          prefix "++" (AstPrefixFunc "++"),
          prefix "+" id,
          prefix "!" (AstPrefixFunc "!")
        ],
        [ postfix "++" (AstPostfixFunc "++"),
          postfix "--" (AstPostfixFunc "--")
        ],
        [ binary "**" (AstBinaryFunc "**"),
          binary "*" (AstBinaryFunc "*"),
          binary "/" (AstBinaryFunc "/"),
          binary "%" (AstBinaryFunc "%")
        ],
        [ binary "+" (AstBinaryFunc "+"),
          binary "-" (AstBinaryFunc "-"),
          binary "|" (AstBinaryFunc "|"),
          binary "&" (AstBinaryFunc "&")
        ],
        [ binary "==" (AstBinaryFunc "=="),
          binary "!=" (AstBinaryFunc "!="),
          binary ">" (AstBinaryFunc ">"),
          binary ">=" (AstBinaryFunc ">="),
          binary "<" (AstBinaryFunc "<"),
          binary "<=" (AstBinaryFunc "<=")
        ],
        [ binary "||" (AstBinaryFunc "||"),
          binary "&&" (AstBinaryFunc "&&")
        ],
        [ ternary AstTernary ],
        [ binary "=" (AstBinaryFunc "=") ]
    ]

pAst :: Parser Ast
pAst = makeExprParser pTerm operatorTable

parseAst :: String -> Either ParserError Ast
parseAst = parse (between sc eof pAst) ""
