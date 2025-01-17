{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.ParserAst (
    -- * Classes
    Ast (..),
    MarylType (..),
    Function (..),
    Variable (..),
    Structure (..),

    -- * Types
    Parser,
    ParserError,

    -- * Functions

    -- ** Utility
    isValidType,
    getMarylType,

    -- ** Main parsing functions
    parseAST,
    types,
    pAst,
    pTerm,
    pExpr,
    pImport,
    pIf,
    pElseIf,
    pElse,
    pReturn,
    pLoop,
    pBreak,
    pList,
    pListElem,
    pEqual,
    pLabel,
    variable,
    pDeclarationVar,
    pFunc,
    pDeclarationFunc,
    pDeclarationStruct,
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
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.Void (Void)
import Debug.Trace
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
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

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

-- | Structure structure containing the name of the structure, its type and its values in a block.
data Structure = Structure
    { sName :: String,
      sProperties :: [Ast]
    }
    deriving (Eq, Ord, Show)

-- | AST values parsed by the program.
data Ast
    = AstVar String
    | AstVoid
    | AstInt Int
    | AstBool Bool
    | AstString String
    | AstChar Char
    | AstDouble Double
    | AstGlobal Ast
    | AstBinaryFunc String Ast Ast
    | AstPostfixFunc String Ast
    | AstPrefixFunc String Ast
    | AstFunc Function
    | -- | if condition do [else if] (Maybe else)
      AstIf Ast Ast [Ast] (Maybe Ast)
    | -- | condition ? do : else
      AstTernary Ast Ast Ast
    | AstReturn Ast
    | AstBlock [Ast]
    | AstStruct [Ast]
    | -- | loopName condition (AstBlock to loop in)
      AstLoop (Maybe String) Ast Ast
    | -- | break statement
      AstBreak (Maybe String)
    | -- | continue statement
      AstContinue (Maybe String)
    | AstDefineVar Variable
    | AstDefineFunc Function
    | AstArg Ast (Maybe Int)
    | AstDefineStruct Structure
    | AstList [Ast]
    | -- | variable indexes
      AstListElem String [Int]
    | -- | label-name value
      AstLabel String Ast
    | -- | file to import, must be .mrl extension
      AstImport String
    deriving (Eq, Ord)

instance Show Ast where
    show :: Ast -> String
    show (AstVar s) = s
    show AstVoid = "Void"
    show (AstInt n) = show n
    show (AstBool b) = show b
    show (AstString s) = show s
    show (AstChar c) = show c
    show (AstDouble d) = show d
    show (AstGlobal ast) = "Global value [" ++ show ast ++ "]"
    show (AstBinaryFunc op left right) = show left ++ " " ++ show op ++ " " ++ show right
    show (AstPostfixFunc f ast) = show ast ++ tail (init (show f))
    show (AstPrefixFunc f ast) = tail (init (show f)) ++ show ast
    show (AstFunc (Function funcName funcArgs funcBody _)) = "call " ++ show funcName ++ "(" ++ show funcArgs ++ "){" ++ show funcBody ++ "}"
    show (AstIf cond ifBlock elseIf maybeElse) = "if (" ++ show cond ++ "){" ++ show ifBlock ++ "} " ++ show elseIf ++ " else {" ++ show maybeElse ++ "}"
    show (AstTernary cond terBlock elseBlock) = show cond ++ " ? " ++ show terBlock ++ " : " ++ show elseBlock
    show (AstReturn ast) = "return " ++ show ast
    show (AstBlock blocks) = show blocks
    show (AstLoop loopName cond loopBlock) = "while(" ++ show cond ++ "){" ++ show loopBlock ++ "} --> [" ++ maybe "" show loopName ++ "]"
    show (AstBreak loopName) = "break(" ++ show loopName ++ ")"
    show (AstContinue loopName) = "continue(" ++ show loopName ++ ")"
    show (AstDefineVar (Variable varName varType varValue)) = show varType ++ " " ++ show varName ++ " = " ++ show varValue
    show (AstDefineFunc (Function name args funcBody typeReturn)) = show typeReturn ++ " " ++ tail (init (show name)) ++ "(" ++ intercalate ", " (map show args) ++ "){" ++ intercalate "; " (map show funcBody) ++ "; }"
    show (AstArg arg idx) = "(Arg " ++ show arg ++ " (" ++ show idx ++ "))"
    show (AstList asts) = "List" ++ show asts
    show (AstListElem var idxs) = show var ++ "[" ++ intercalate "][" (map show idxs) ++ "]"
    show (AstStruct s) = "Struct " ++ show s
    show (AstDefineStruct s) = "DefStruct " ++ sName s ++ " " ++ show (sProperties s)
    show (AstLabel n v) = "Label " ++ n ++ ": " ++ show v
    show (AstImport f) = "Import " ++ f

-- | Types handled by the program.
data MarylType = String | Int | Double | Char | Bool | Void | List MarylType | Const MarylType | Struct String | Undefined
    deriving (Eq, Ord, Show)

-- | Compare AST to a MarylType and evaluates with Boolean
isValidType :: Ast -> MarylType -> Bool
isValidType AstVoid Void = True
isValidType (AstInt _) Int = True
isValidType (AstBool _) Bool = True
isValidType (AstString _) String = True
isValidType (AstChar _) Char = True
isValidType (AstDouble _) Double = True
isValidType _ _ = False
{- ^ ^^
 doesn't handle AstStruct
                AstList
                AstListElem
                AstArg
-}

-- | Obtain suggested MarylType from an AST
getMarylType :: Ast -> MarylType
getMarylType AstVoid = Void
getMarylType (AstInt _) = Int
getMarylType (AstBool _) = Bool
getMarylType (AstString _) = String
getMarylType (AstChar _) = Char
getMarylType (AstDouble _) = Double
getMarylType _ = Undefined

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

rWords :: [String]
rWords =
    types'
        ++ [ "while",
             "if",
             "else",
             "true",
             "false",
             "return",
             "null",
             "const",
             "struct",
             "break",
             "continue"
           ]

-- | Variable names must start with a letter or an underscore ([_a-zA-Z]), and can be followed by any alphanumerical character or underscore ([_a-zA-Z0-9])
variable :: Parser String
variable = variable' >>= check
  where
    check x =
        if x `elem` rWords
            then fail $ show x ++ " is a reserved identifier"
            else return x

variable' :: Parser String
variable' =
    (:)
        <$> (try letterChar <|> bonusChar)
        <*> many (alphaNumChar <|> bonusChar)
        <?> "variable"

integer :: Parser Int
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

{- | Parsing labels like so: name:

>>> struct vector vect = {x: 42, y:1};
-}
pLabel :: Parser Ast
pLabel = do
    n <- variable
    sc
    _ <- symbol ":"
    sc
    AstLabel n <$> convertValue

{- | Parsing import statement to import another Maryl file content, must be formatted: import "filepath";

>>> import "toto.mrl";
-}
pImport :: Parser Ast
pImport = lexeme $ string "import" >> sc >> AstImport <$> stringLiteral

listElem :: Parser Int
listElem = between (symbol "[") (symbol "]") integer

listElem' :: Parser [Int]
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

    Int (23)

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
        [ try pLabel,
          AstDouble <$> try double,
          AstInt <$> integer,
          try pListElem,
          AstList <$> try pList,
          AstBool <$> bool,
          AstChar <$> charLiteral,
          AstString <$> stringLiteral,
          AstStruct <$> try struct,
          AstBlock <$> block,
          try pFunc,
          AstVoid <$ try (lexeme $ string "null"),
          AstVar <$> lexeme variable
        ]

-- | Parsing lists of values between brackets ([]) and separated by a comma (,) if there are multiple values: [1] or [1, 2, 3, 4].
pList :: Parser [Ast]
pList = between (symbol "[") (symbol "]") (convertValue `sepBy` lexeme ",")

list :: Parser Ast
list = list' pExpr

list' :: Parser Ast -> Parser Ast
list' = between (symbol "(") (symbol ")")

listVariables :: Parser [Ast]
listVariables = between (symbol "(") (symbol ")") (pExpr `sepBy` lexeme ",")

listVariables' :: Parser [Ast]
listVariables' =
    between
        (symbol "(")
        (symbol ")")
        (pDeclarationVar `sepBy` lexeme ",")

block :: Parser [Ast]
block = between (symbol "{") (symbol "}") (many pTerm)

struct :: Parser [Ast]
struct = between (symbol "{") (symbol "}") (convertValue `sepBy` lexeme ",")

types' :: [String]
types' =
    [ "int",
      "float",
      "string",
      "char",
      "bool",
      "void"
    ]

pStruct :: Parser String
pStruct = lexeme $ do
    s <- string "struct"
    sc
    n <- variable
    return $ s ++ " " ++ n

pListType :: Parser String
pListType = lexeme $ do
    s <- some $ string "[]"
    t <- choice $ map string types'
    return (concat s ++ t)

types :: Parser String
types =
    choice
        [ try pListType,
          choice (map string $ ("const " ++) <$> types'),
          choice (map string types'),
          pStruct
        ]

trimFront :: String -> String -> String
trimFront toTrim str = dropWhile (\x -> x == ' ' || x == '\t') (fromJust $ stripPrefix toTrim str)

-- | Returns a 'MarylType' based on string given as parameter. If the string is not supported, returns 'Undefined'.
getType :: String -> MarylType
getType "int" = Int
getType "float" = Double
getType "string" = String
getType "char" = Char
getType "bool" = Bool
getType "void" = Void
getType str
    | "[]" `isPrefixOf` trace ("prefix of " ++ str) str = List $ getType (fromJust $ stripPrefix "[]" str)
    | "const" `isPrefixOf` str = Const $ getType $ trimFront "const" str
    | "struct" `isPrefixOf` str = Struct $ takeWhile (\x -> x /= ' ' && x /= '\t') $ trimFront "struct" str
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

{- | Parsing 'pStruct' declaration, formatted: struct name {}

>>> struct vector {int x; int y;}
-}
pDeclarationStruct :: Parser Ast
pDeclarationStruct = do
    symbol "struct" >> sc
    n <- variable
    sc
    b <- block
    return $ AstDefineStruct (Structure {sName = n, sProperties = b})

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
    return $ AstLoop Nothing cond toDo

pVoid :: Parser Ast
pVoid = list' pVoid' <|> pVoid'

pVoid' :: Parser Ast
pVoid' = AstVoid <$ ""

{- | Parsing return statement formatted like: return val; or return;

>>> return (1 + 1);

>>> return foo();

>>> return;
-}
pReturn :: Parser Ast
pReturn = string "return" >> sc >> (try pExpr <|> pVoid)

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
pBreak = lexeme $ AstBreak Nothing <$ string "break"

{- | Parsing continue statement (just a "continue" keyword).

>>> while (true) {continue;}
-}
pContinue :: Parser Ast
pContinue = lexeme $ AstContinue Nothing <$ string "continue"

eqSymbol :: Parser String
eqSymbol =
    choice
        ( symbol
            <$> [ "=",
                  "+=",
                  "-=",
                  "**=",
                  "*=",
                  "/=",
                  "%=",
                  "|=",
                  "&=",
                  "^=",
                  ">>=",
                  "<<="
                ]
        )

{- | Parsing equal symbols for variable value assignation, syntax being: variable = value;

    Assignations handled:

    = -> standard assignation

    += -> addition assignation

    -= -> subtraction assignation

    *= -> multiplication assignation

    /= -> division assignation

    **= -> power assignation

    %= -> modulo assignation

    |= -> bitwise OR assignation

    &= -> bitwise AND assignation

    ^= -> bitwise XOR assignation

    >>= -> bitshift right assignation

    <<= -> bitshift left assignation
-}
pEqual :: Parser Ast
pEqual = do
    var <- try pListElem <|> (AstVar <$> lexeme variable)
    sc
    eq <- eqSymbol
    sc
    AstBinaryFunc eq var <$> pExpr

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
          pImport <* semi,
          pIf,
          pLoop,
          try pDeclarationFunc,
          try pDeclarationStruct,
          pDeclarationVar <* semi,
          pBreak <* semi,
          pContinue <* semi,
          try pEqual <* semi,
          pExpr <* semi
        ]

-- | Megaparsec's InfixL wrapper
binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL (f <$ symbol n)

-- | Megaparsec's InfixR wrapper
binary' :: String -> (a -> a -> a) -> Operator Parser a
binary' n f = InfixR (f <$ symbol n)

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
    [ [binary "." (AstBinaryFunc ".")],
        [ prefix "--" (AstPrefixFunc "--"),
          prefix "-" (AstPrefixFunc "-"),
          prefix "++" (AstPrefixFunc "++"),
          prefix "+" id,
          prefix "!" (AstPrefixFunc "!"),
          prefix "~" (AstPrefixFunc "~")
        ],
        [ postfix "++" (AstPostfixFunc "++"),
          postfix "--" (AstPostfixFunc "--")
        ],
      [binary' "**" (AstBinaryFunc "**")],
        [ binary "*" (AstBinaryFunc "*"),
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
      [ternary AstTernary]
    ]

-- | Megaparsec Expr parser call with 'convertValue' defining the types to parse and 'operatorTable' containing all operators handled.
pExpr :: Parser Ast
pExpr = makeExprParser pExpr' operatorTable

pExpr' :: Parser Ast
pExpr' = list' pExpr <|> convertValue

-- | 'parseAST' entry function parsing multiple AST as defined by 'pTerm'
pAst :: Parser [Ast]
pAst = many pTerm

{-  | Main parsing function returning a list of parsed AST, or a Megaparsec formatted error.
  Takes the string to parse as parameter.
-}
parseAST :: String -> Either ParserError [Ast]
parseAST = parse (between sc eof pAst) ""
