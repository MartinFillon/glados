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
import Control.Monad ((>=>))
import qualified Control.Monad as Monad
import Data.Functor ((<&>))
import Debug.Trace (trace)
import Eval.Evaluator (evalAST)
import GHC.GHCi.Helpers (flushAll)
import Memory (Memory, initMemory)
import Parsing.ParserAst (Ast (..), parseAST)
import System.IO (hIsTerminalDevice, isEOF, stdin)
import Utils (handleParseError, pError)
import VirtualMachine (vm)

handleEvalResult :: [Ast] -> Either String ([Ast], Memory) -> Maybe FilePath -> IO ()
handleEvalResult originalAst (Right (_, mem)) (Just o) =
    let (_, updatedMem) = translateToASM originalAst mem
     in writeInstructionsToFile o updatedMem
            >> putStrLn ("Maryl ASM produced in " ++ o)
handleEvalResult originalAst (Right (_, mem)) Nothing =
    let (_, updatedMem) = translateToASM originalAst mem
     in writeInstructionsToFile "out.masm" updatedMem
            >> putStrLn "Maryl ASM produced in out.masm"
handleEvalResult _ (Left err) _ =
    pError ("*** ERROR *** with\n\t" ++ err)

parseAstCode :: Memory -> Maybe FilePath -> [Ast] -> IO Memory
parseAstCode mem out asts =
    let evalResult = evalAST mem asts
     in handleEvalResult asts evalResult out
            >> return (either (const mem) snd evalResult)

isImport :: Ast -> Bool
isImport (AstImport _) = True
isImport _ = False

handleImports' :: [String] -> IO [Ast]
handleImports' [] = return []
handleImports' (x : xs) = do
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

parseSourceCode :: Memory -> String -> Maybe FilePath -> IO Memory
parseSourceCode mem s out =
    handleParseError True (parseAST s) >>= (handleImports >=> parseAstCode mem out)

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

handleInput :: Memory -> Maybe FilePath -> String -> IO Memory
handleInput m o s = parseSourceCode m (normalizeTabs s) o

getContentFromFile :: Memory -> String -> Maybe FilePath -> IO Memory
getContentFromFile mem filepath out =
    readFile filepath >>= handleInput mem out

countChar :: Char -> String -> Int
countChar c s = length (filter (== c) s)

countBrackets :: String -> Bool
countBrackets s = countChar '{' s == countChar '}' s

checkBuf' :: Memory -> String -> IO (String, Memory)
checkBuf' mem s
    | countBrackets s = handleInput mem Nothing s >>= \newMem -> return ("", newMem)
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

glados :: Mode -> IO ()
glados (Compile (Just path) out) = Monad.void (getContentFromFile initMemory path out)
glados (Compile Nothing _) = getContentFromStdin initMemory
glados (Vm path a) = vm path a
