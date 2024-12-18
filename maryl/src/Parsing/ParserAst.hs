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
    pTest,
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
    (<|>), optional,
 )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

data Ast
    = AstVar String
    | AstInt Integer
    | AstBool Bool
    | AstString String
    | AstChar Char
    | AstDouble Double
    | AstNegation Ast
    | AstInc Ast
    | AstDec Ast
    | AstList Ast
    | AstSum Ast Ast
    | AstSubtr Ast Ast
    | AstProduct Ast Ast
    | AstDivision Ast Ast
    | AstModulo Ast Ast
    | AstExp Ast Ast
    | AstNot Ast
    | AstEq Ast Ast
    | AstNotEq Ast Ast
    | AstLower Ast Ast
    | AstLowerEq Ast Ast
    | AstHigher Ast Ast
    | AstHigherEq Ast Ast
    | AstOr Ast Ast
    | AstBinOr Ast Ast
    | AstAnd Ast Ast
    | AstBinAnd Ast Ast
    | AstIf Ast Ast [Ast] (Maybe Ast) -- if cond do [else if] (Maybe else)
    | AstTernary Ast Ast Ast -- cond ? do : else
    | AstReturn Ast
    | AstBlock [Ast]
    | AstDefine Ast Ast -- first must be AstVar
    | AstDefinePlus Ast Ast
    | AstDefineMinus Ast Ast
    | AstDefineMul Ast Ast
    | AstDefineDiv Ast Ast
    deriving (Eq, Ord, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

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

variable :: Parser String
variable = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

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

block :: Parser [Ast]
block = between (symbol "{") (symbol "}") (many pAst)

pReturn :: Parser Ast
pReturn = string "return" >> sc >> pAst

pTest :: Parser String
pTest = try $ string "test"

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
        [ pIf,
          list pAst,
          convertValue
        ]

binary :: String -> (Ast -> Ast -> Ast) -> Operator Parser Ast
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: String -> (Ast -> Ast) -> Operator Parser Ast
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

ternary :: (Ast -> Ast -> Ast -> Ast) -> Operator Parser Ast
ternary f = TernR ((f <$ lexeme (char ':')) <$ lexeme (char '?'))

operatorTable :: [[Operator Parser Ast]]
operatorTable =
    [   [ prefix "--" AstDec,
          prefix "-" AstNegation,
          prefix "++" AstInc,
          prefix "+" id,
          prefix "!" AstNot
        ],
        [ postfix "++" AstInc,
          postfix "--" AstDec
        ],
        [ binary "**" AstExp,
          binary "*" AstProduct,
          binary "/" AstDivision,
          binary "%" AstModulo
        ],
        [ binary "+" AstSum,
          binary "-" AstSubtr,
          binary "|" AstBinOr,
          binary "&" AstBinAnd
        ],
        [ binary "==" AstEq,
          binary "!=" AstNotEq,
          binary ">" AstHigher,
          binary ">=" AstHigherEq,
          binary "<" AstLower,
          binary "<=" AstLowerEq
        ],
        [ binary "||" AstOr,
          binary "&&" AstAnd
        ],
        [ ternary AstTernary ],
        [ binary "=" AstDefine,
          binary "+=" AstDefinePlus,
          binary "-=" AstDefineMinus,
          binary "*=" AstDefineMul,
          binary "/=" AstDefineDiv
        ]
    ]

pAst :: Parser Ast
pAst = makeExprParser pTerm operatorTable

parseAst :: String -> Either ParserError Ast
parseAst = parse (between sc eof pAst) ""
