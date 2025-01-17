{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}

module Glados (glados) where

import ArgsHandling (Mode (..))
import Compiler.Translation.ASTtoASM (translateToASM)
import Compiler.WriteASM (writeInstructionsToFile)
import qualified Control.Monad as Monad
import Eval.Evaluator (evalAST)
import GHC.GHCi.Helpers (flushAll)
import Memory (Memory, initMemory)
import Parsing.ParserAst (Ast (..), parseAST)
import System.IO (hIsTerminalDevice, isEOF, stdin)
import Utils (handleParseError, pError)
import VirtualMachine (vm)
import Data.Functor ((<&>))
import Debug.Trace (trace)
import Control.Monad ((>=>))

handleEvalResult :: [Ast] -> Either String ([Ast], Memory) -> IO ()
handleEvalResult originalAst (Right (_, mem)) =
    let (_, updatedMem) = translateToASM originalAst mem
     in writeInstructionsToFile "out.masm" updatedMem
            >> putStrLn "Maryl ASM produced in out.masm"
handleEvalResult _ (Left err) =
    pError ("*** ERROR *** with\n\t" ++ err)

parseAstCode :: Memory -> [Ast] -> IO Memory
parseAstCode mem asts = let evalResult = evalAST mem asts
         in handleEvalResult asts evalResult
                >> return (either (const mem) snd evalResult)

isImport :: Ast -> Bool
isImport (AstImport _) = True
isImport _ = False

handleImports' :: [String] -> IO [Ast]
handleImports' [] = return []
handleImports' (x:xs) = do
    content <- readFile x
    case parseAST content of
        Left err -> handleParseError True (Left err)
        Right asts -> do
            handled <- handleImports asts
            next <- handleImports' xs
            return $ handled ++ next

handleImports :: [Ast] -> IO [Ast]
handleImports asts = case filter isImport asts of
    [] -> return $ filter (not . isImport) asts
    imports -> handleImports' (getImportFile <$> imports) <&> (\imported -> filter (not . isImport) imported ++ asts)

getImportFile :: Ast -> String
getImportFile (AstImport file) = file
getImportFile _ = ""

parseSourceCode :: Memory -> String -> IO Memory
parseSourceCode mem s =
    handleParseError True (parseAST s) >>= (handleImports >=> parseAstCode mem)

normalizeTabs :: String -> String
normalizeTabs [] = []
normalizeTabs (' ' : xs) = detectSpaces 1 xs
normalizeTabs ('\t' : xs) = '\t' : normalizeTabs xs
normalizeTabs (x : xs) = x : normalizeTabs xs

detectSpaces :: Int -> String -> String
detectSpaces count [] = replicate count ' '
detectSpaces count (' ' : xs)
    | count == 3 = '\t' : normalizeTabs xs
    | otherwise = detectSpaces (count + 1) xs
detectSpaces count (x : xs) =
    replicate count ' ' ++ x : normalizeTabs xs

handleInput :: Memory -> String -> IO Memory
handleInput m s = parseSourceCode m (normalizeTabs s)

getContentFromFile :: Memory -> String -> IO Memory
getContentFromFile mem filepath =
    readFile filepath >>= handleInput mem

countChar :: Char -> String -> Int
countChar c s = length (filter (== c) s)

countBrackets :: String -> Bool
countBrackets s = countChar '{' s == countChar '}' s

checkBuf' :: Memory -> String -> IO (String, Memory)
checkBuf' mem s
    | countBrackets s = handleInput mem s >>= \newMem -> return ("", newMem)
    | otherwise = return (s, mem)

checkBuf :: Memory -> String -> String -> IO (String, Memory)
checkBuf mem s i
    | s /= "" = checkBuf' mem (s ++ '\n' : i)
    | otherwise = checkBuf' mem i

-- Prints prompt if it's a TTY
getLineFromStdin' :: Memory -> String -> Bool -> Bool -> IO ()
getLineFromStdin' _ _ _ True = return ()
getLineFromStdin' mem s b False =
    getLine
        >>= ( checkBuf mem s
                Monad.>=> (\(newBuf, newMem) -> getLineFromStdin newMem newBuf b)
            )

getLineFromStdin :: Memory -> String -> Bool -> IO ()
getLineFromStdin mem s True =
    putStr "> "
        >> flushAll
        >> isEOF
        >>= getLineFromStdin' mem s True
getLineFromStdin mem s False =
    isEOF >>= getLineFromStdin' mem s False

getContentFromStdin :: Memory -> IO ()
getContentFromStdin mem =
    hIsTerminalDevice stdin
        >>= getLineFromStdin mem ""

glados :: Mode -> [String] -> IO ()
glados Compile (filepath : _) = Monad.void (getContentFromFile initMemory filepath)
glados Compile [] = getContentFromStdin initMemory
glados Vm x = vm x
