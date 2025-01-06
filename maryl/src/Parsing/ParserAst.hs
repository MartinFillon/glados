{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParserAst (
    -- * Classes
    Ast (..),
    MarylType (..),
    Function (..),
    Variable (..),
    -- * Types
    Parser,
    ParserError,
    -- * Functions
    -- ** Main parsing functions
    parseAST,
    pAst,
    pTerm,
    pExpr,
    pIf,
    pElseIf,
    pElse,
    pReturn,
    pLoop,
    pBreak,
    pList,
    pListElem,
    variable,
    pDeclarationVar,
    pFunc,
    pDeclarationFunc,
    operatorTable,
    convertValue,
    getType,
    -- ** Megaparsec functions wrappers
    binary,
    prefix,
    postfix,
    ternary,
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (
    Operator (..),
    makeExprParser,
 )
import Data.Maybe (fromMaybe, fromJust)
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
import Text.Megaparsec.Char (char, letterChar, string, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (stripPrefix, isPrefixOf)

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

-- | Types handled by the program.
data MarylType = String | Integer | Double | Char | Bool | Void | List MarylType | Const MarylType | Undefined
    deriving (Eq, Ord, Show)

-- | Function structure containing the name of the function, its arguments, the content of the function, and the return value of the function.
data Function = Function
    { fName :: String,
      fArgs :: [Ast],
      fBody :: [Ast],
      fType :: MarylType
    }
    deriving (Eq, Ord, Show)

-- | Variable structure containing the name of the variable, its type and its value.
data Variable = Variable
    { vName :: String,
      vType :: MarylType,
      vValue :: Ast
    }
    deriving (Eq, Ord, Show)

-- | AST values parsed by the program.
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
    | AstIf Ast Ast [Ast] (Maybe Ast) -- ^ if condition do [else if] (Maybe else)
    | AstTernary Ast Ast Ast -- ^ condition ? do : else
    | AstReturn Ast
    | AstBlock [Ast]
    | AstLoop Ast Ast -- ^ condition (AstBlock to loop in)
    | AstBreak -- ^ break statement
    | AstDefineVar Variable
    | AstDefineFunc Function
    | AstList [Ast]
    | AstListElem String [Integer] -- ^ variable indexes
    deriving (Eq, Ord, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

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

-- | Variable names must start with a letter or an underscore ([_a-zA-Z]), and can be followed by any alphanumerical character or underscore ([_a-zA-Z0-9])
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

listElem :: Parser Integer
listElem = between (symbol "[") (symbol "]") integer

listElem' :: Parser [Integer]
listElem' = between (symbol "[") (symbol "]") (integer `sepBy` lexeme ",")

-- | Parsing access to an element of a list formatted: foo[index]. Multiple dimensions can be accessed by adding the index after, formatted like so: foo[i][j] or foo[i,j].
pListElem :: Parser Ast
pListElem = do
    v <- variable
    i <- try (some listElem) <|> listElem'
    return $ AstListElem v i

{- |
    Handled values for parsing.

    Double (0.42)

    Integer (23)

    List element (foo[0])

    List ([0,1,2])

    Boolean (true | false)

    Character ('a')

    String ("abc")

    Block of instructions ({int foo = 1; return foo;})

    Function call (bar() | baz(true, 2))

    Variable (toto | _titi | tata42)
-}
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

-- | Parsing lists of values between brackets ([]) and separated by a comma (,) if there are multiple values: [1] or [1, 2, 3, 4].
pList :: Parser [Ast]
pList = between (symbol "[") (symbol "]") (convertValue `sepBy` lexeme ",")

list :: Parser Ast
list = between (symbol "(") (symbol ")") pExpr

list' :: Parser Ast -> Parser Ast
list' = between (symbol "(") (symbol ")")

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

-- | Returns a 'MarylType' based on string given as parameter. If the string is not supported, returns 'Undefined'.
getType :: String -> MarylType
getType "int" = Integer
getType "float" = Double
getType "string" = String
getType "char" = Char
getType "bool" = Bool
getType "void" = Void
getType str
    | "[]" `isPrefixOf` str = List $ getType (fromJust $ stripPrefix "[]" str)
    | "const" `isPrefixOf` str = Const $ getType (dropWhile (\x -> x == ' ' || x == '\t') (fromJust $ stripPrefix "const" str))
    | otherwise = Undefined

optionalValue :: Parser (Maybe Ast)
optionalValue = optional $ do
    sc
    _ <- string "="
    sc
    pExpr

{- | Parsing 'variable' declaration, formatted: type name; or type name = value;

>>> int foo = 1;
-}
pDeclarationVar :: Parser Ast
pDeclarationVar = do
    t <- types
    sc
    n <- variable
    v <- optionalValue
    return $
        AstDefineVar
            (Variable {vName = n, vType = getType t, vValue = fromMaybe AstVoid v})

{- | Parsing 'pFunc' declaration, formatted: type name() {}

>>> int one() {return 1;}

>>> bool isLower(int a, int b) {return a < b;}
-}
pDeclarationFunc :: Parser Ast
pDeclarationFunc = do
    t <- types
    sc
    n <- variable
    a <- listVariables'
    b <- block
    return $
        AstDefineFunc (Function {fName = n, fArgs = a, fBody = b, fType = getType t})

{- | Function names must be formatted like a 'variable', followed by parenthesis: foo()

>>> foo();

>>> add(1, 2);
-}
pFunc :: Parser Ast
pFunc = do
    n <- variable
    a <- listVariables
    return $ AstFunc (Function {fName = n, fArgs = a, fBody = [], fType = Void})

{- | Parsing loops with keyword "while", followed by a boolean condition between parenthesis and a block of instructions: while (boolean) {}

>>> while (true) {print("looping once."); break;}

>>> while (i < 10) {print(i); i++;}
-}
pLoop :: Parser Ast
pLoop = do
    string "while" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    return $ AstLoop cond toDo

pVoid :: Parser Ast
pVoid = AstVoid <$ ""

{- | Parsing return statement formatted like: return val; or return;

>>> return (1 + 1);

>>> return foo();

>>> return;
-}
pReturn :: Parser Ast
pReturn = string "return" >> sc >> (try pFunc <|> list <|> pExpr <|> pVoid)

-- | Parsing else statement formatted with the "else" keyword followed by a block: else {}
pElse :: Parser (Maybe Ast)
pElse = optional $ string "else" >> sc >> AstBlock <$> block >>= \b -> return b

-- | Parsing else if statement formatted with the "else if" keyword followed by a boolean condition and a block: else if (boolean) {}
pElseIf :: Parser Ast
pElseIf = try $ do
    string "else if" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    return $ AstIf cond toDo [] Nothing

{- | Parsing else if statement formatted with the "if" keyword followed by a boolean condition and a block: if (boolean) {}. Can be followed by one or more else if's and an else: if () {} else if () {} else {}

>>> if (true) {return true;}

>>> if (foo > 0) {return 1;} else if (foo < 0) {return -1;} else {return 0;}
-}
pIf :: Parser Ast
pIf = do
    string "if" >> sc
    cond <- list
    toDo <- AstBlock <$> block
    elseIf <- many pElseIf
    AstIf cond toDo elseIf <$> pElse

{- | Parsing break statement (just a "break" keyword).

>>> while (true) {break;}
-}
pBreak :: Parser Ast
pBreak = lexeme $ AstBreak <$ string "break"

{- |
    Parsing statements

    Return

    If

    Loop

    Function declaration

    Variable declaration

    Break

    Expression (binary, prefix, postfix, ternary)
-}
pTerm :: Parser Ast
pTerm =
    choice
        [ AstReturn <$> (pReturn <* semi),
          try pIf,
          try pLoop,
          try pDeclarationFunc,
          try pDeclarationVar <* semi,
          try list,
          try pBreak <* semi,
          pExpr <* semi
        ]

-- | Megaparsec's InfixL wrapper
binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL (f <$ symbol n)

-- | Megaparsec's Prefix and Postfix wrapper
prefix, postfix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix (f <$ symbol n)
postfix n f = Postfix (f <$ symbol n)

-- | Megaparsec's TernR wrapper
ternary :: (a -> a -> a -> a) -> Operator Parser a
ternary f = TernR ((f <$ lexeme (char ':')) <$ lexeme (char '?'))

-- | Operator table containing every operator handled by the program.
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
        [ binary "=" (AstBinaryFunc "="),
          binary "+=" (AstBinaryFunc "+="),
          binary "-=" (AstBinaryFunc "-="),
          binary "*=" (AstBinaryFunc "*="),
          binary "/=" (AstBinaryFunc "/="),
          binary "|=" (AstBinaryFunc "|="),
          binary "&=" (AstBinaryFunc "&="),
          binary "^=" (AstBinaryFunc "^="),
          binary ">>=" (AstBinaryFunc ">>="),
          binary "<<=" (AstBinaryFunc "<<=")
        ]
    ]

-- | Megaparsec Expr parser call with 'convertValue' defining the types to parse and 'operatorTable' containing all operators handled.
pExpr :: Parser Ast
pExpr = makeExprParser convertValue operatorTable

-- | 'parseAST' entry function parsing multiple AST as defined by 'pTerm'
pAst :: Parser [Ast]
pAst = many $ try pTerm

-- | Main parsing function returning a list of parsed AST, or a Megaparsec formatted error.
-- Takes the string to parse as parameter.
parseAST :: String -> Either ParserError [Ast]
parseAST = parse (between sc eof pAst) ""
