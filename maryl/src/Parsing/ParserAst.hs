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
    pList,
    pExpr,
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
    parseAST,
    ternary,
    listVariables,
    listVariables',
    Ast (..),
    Function (..),
    Variable (..),
    MarylType (..),
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (
    Operator (..),
    makeExprParser,
 )
import Data.Maybe (fromMaybe)
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
    optional,
    parse,
    sepBy,
    some,
    (<?>),
    (<|>),
 )
import Text.Megaparsec.Char (char, letterChar, space1, string, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

data MarylType = String | Integer | Double | Char | Bool | Void | List MarylType | Undefined
    deriving (Eq, Ord, Show)

data Function = Function
    { fName :: String,
      fArgs :: [Ast],
      fBody :: [Ast],
      fType :: MarylType
    }
    deriving (Eq, Ord, Show)

data Variable = Variable
    { vName :: String,
      vType :: MarylType,
      vValue :: Ast
    }
    deriving (Eq, Ord, Show)

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
    | AstDefineVar Variable
    | AstDefineFunc Function
    | AstList [Ast]
    | AstListElem String Integer -- variable index
    deriving (Eq, Ord, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc =
    L.space (void $ some (char ' ' <|> char '\t' <|> char '\n')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

semi :: Parser String
semi = symbol ";"

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

bonusChar' :: String
bonusChar' = "_"

bonusChar :: Parser Char
bonusChar = choice $ char <$> bonusChar'

variable :: Parser String
variable =
    (:)
        <$> (try letterChar <|> bonusChar)
        <*> many (alphaNumChar <|> bonusChar)
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

pListElem :: Parser Ast
pListElem = do
    v <- variable
    _ <- symbol "["
    i <- integer
    _ <- symbol "]"
    return $ AstListElem v i

convertValue :: Parser Ast
convertValue =
    choice
        [ AstDouble <$> try double,
          AstInt <$> integer,
          try pListElem,
          AstList <$> try pList,
          AstBool <$> bool,
          AstChar <$> charLiteral,
          AstString <$> stringLiteral,
          AstBlock <$> block,
          try pFunc,
          AstVar <$> lexeme variable
        ]

pList :: Parser [Ast]
pList = between (symbol "[") (symbol "]") (convertValue `sepBy` lexeme ",")

list :: Parser Ast
list = between (symbol "(") (symbol ")") pExpr

listVariables :: Parser [Ast]
listVariables = between (symbol "(") (symbol ")") (convertValue `sepBy` lexeme ",")

listVariables' :: Parser [Ast]
listVariables' =
    between
        (symbol "(")
        (symbol ")")
        ((types >> sc >> convertValue) `sepBy` lexeme ",")

block :: Parser [Ast]
block = between (symbol "{") (symbol "}") (many pTerm)

types' :: [String]
types' =
    [ "int",
      "float",
      "string",
      "char",
      "bool",
      "void"
    ]

types :: Parser String
types = choice (map string (("[]" ++) <$> types')) <|> choice (map string types')

getType :: String -> MarylType
getType "int" = Integer
getType "float" = Double
getType "string" = String
getType "char" = Char
getType "bool" = Bool
getType "void" = Void
getType ('[' : ']' : t) = List $ getType t
getType _ = Undefined

optionalValue :: Parser (Maybe Ast)
optionalValue = optional $ do
    sc
    _ <- string "="
    sc
    pExpr

pDeclarationVar :: Parser Ast
pDeclarationVar = do
    t <- types
    sc
    n <- variable
    v <- optionalValue
    return $
        AstDefineVar
            (Variable {vName = n, vType = getType t, vValue = fromMaybe AstVoid v})

pDeclarationFunc :: Parser Ast
pDeclarationFunc = do
    t <- types
    sc
    n <- variable
    a <- listVariables'
    b <- block
    return $
        AstDefineFunc (Function {fName = n, fArgs = a, fBody = b, fType = getType t})

pFunc :: Parser Ast
pFunc = do
    n <- variable
    a <- listVariables
    return $ AstFunc (Function {fName = n, fArgs = a, fBody = [], fType = Void})

pLoop :: Parser Ast
pLoop = do
    string "while" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    return $ AstLoop cond toDo

pReturn :: Parser Ast
pReturn = string "return" >> sc >> (try pFunc <|> pExpr)

pElse :: Parser (Maybe Ast)
pElse = optional $ string "else" >> sc >> AstBlock <$> block >>= \b -> return b

pElseIf :: Parser Ast
pElseIf = try $ do
    string "else if" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    return $ AstIf cond toDo [] Nothing

pIf :: Parser Ast
pIf = do
    string "if" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    elseIf <- many pElseIf
    AstIf cond toDo elseIf <$> pElse

pTerm :: Parser Ast
pTerm =
    choice
        [ AstReturn <$> (pReturn <* semi),
          try pIf,
          try pLoop,
          try pDeclarationFunc,
          try pDeclarationVar <* semi,
          try list,
          pExpr <* semi
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
          prefix "!" (AstPrefixFunc "!"),
          prefix "~" (AstPrefixFunc "~")
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
          binary "&" (AstBinaryFunc "&"),
          binary ">>" (AstBinaryFunc ">>"),
          binary "<<" (AstBinaryFunc "<<"),
          binary "^" (AstBinaryFunc "^")
        ],
        [ binary "==" (AstBinaryFunc "=="),
          binary "!=" (AstBinaryFunc "!="),
          binary ">" (AstBinaryFunc ">"),
          binary ">=" (AstBinaryFunc ">="),
          binary "<" (AstBinaryFunc "<"),
          binary "<=" (AstBinaryFunc "<=")
        ],
        [ binary "or" (AstBinaryFunc "or"),
          binary "and" (AstBinaryFunc "and")
        ],
        [ternary AstTernary],
        [binary "=" (AstBinaryFunc "=")]
    ]

pExpr :: Parser Ast
pExpr = makeExprParser convertValue operatorTable

pAst :: Parser [Ast]
pAst = many $ try pTerm

parseAST :: String -> Either ParserError [Ast]
parseAST = parse (between sc eof pAst) ""
