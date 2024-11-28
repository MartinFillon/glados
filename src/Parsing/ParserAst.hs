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
    operatorTable,
    binary,
    prefix,
    postfix,
    charLiteral,
    stringLiteral,
    integer,
    double,
    pKeyword,
    sc,
    scn,
    lexeme,
    symbol,
    convertValue,
    parseAst
) where

import Data.Void ( Void )
import Data.Text ( Text )
import Text.Megaparsec ( Parsec, choice, many, between, (<?>), manyTill, some,
    (<|>), empty, MonadParsec (try, eof), parse, ParseErrorBundle )
import Text.Megaparsec.Char ( string, letterChar, alphaNumChar, char, space1 )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(..) )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

data Ast =
    AstVar String
    | AstInt Integer
    | AstBool Bool
    | AstString String
    | AstChar Char
    | AstDouble Double
    | AstNegation Ast
    | AstList Ast
    | AstSum Ast Ast
    | AstSubtr Ast Ast
    | AstProduct Ast Ast
    | AstDivision Ast Ast
    | AstModulo Ast Ast
    | AstEq Ast Ast
    | AstLower Ast Ast
    deriving (Eq, Ord, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
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
bool = lexeme $ choice
    [ False <$ string "#f"
    , True <$ string "#t"
    ]

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)

convertValue :: Parser Ast
convertValue = choice
    [ AstDouble <$> try double
    , AstInt <$> integer
    , AstBool <$> bool
    , AstChar <$> charLiteral
    , AstString <$> stringLiteral
    , AstVar <$> variable
    ]

list :: Parser Ast -> Parser Ast
list = between (symbol "(") (symbol ")")

pTerm :: Parser Ast
pTerm = choice
    [ list pAst
    , convertValue
    ]

binary :: Text -> (Ast -> Ast -> Ast) -> Operator Parser Ast
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Ast -> Ast) -> Operator Parser Ast
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser Ast]]
operatorTable =
    [
        [ prefix "-" AstNegation
        , prefix "+" id ]
    ,   [ binary "*" AstProduct
        , binary "div" AstDivision
        , binary "mod" AstModulo ]
    ,   [ binary "+" AstSum
        , binary "-" AstSubtr ]
    ,   [ binary "eq?" AstEq
        , binary "<" AstLower ]
    ]

pAst :: Parser Ast
pAst = makeExprParser pTerm operatorTable

parseAst :: Text -> Either ParserError Ast
parseAst = parse (between sc eof pAst) ""
