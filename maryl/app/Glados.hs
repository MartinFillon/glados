{-
-- EPITECH PROJECT, 2024
-- gladdos
-- File description:
-- Gladdos
-}
{-# LANGUAGE LambdaCase #-}

module Glados (glados) where

import ArgsHandling (Mode (..))
import Compiler.Translation.ASTtoASM (translateToASM)
import Compiler.WriteASM (writeInstructionsToFile)
import Control.Monad ((>=>))
import qualified Control.Monad as Monad
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Eval.Evaluator (evalAST)
import Memory (Memory, initMemory)
import Parsing.ParserAst (Ast (..), parseAST)
import Printer (Style (..), getColorsFromConf, reset)
import Utils (handleParseError, pError)
import VirtualMachine (vm)

displayError :: String -> IO ()
displayError str =
    getColorsFromConf >>= \case
        Just (_, e, _) ->
            pError $
                show e
                    ++ show Bold
                    ++ "*** ERROR *** with\n"
                    ++ reset
                    ++ show Bold
                    ++ str
                    ++ reset
        Nothing -> pError $ "*** ERROR *** with\n" ++ str

handleEvalResult ::
    [Ast] -> Either String ([Ast], Memory) -> Maybe FilePath -> IO ()
handleEvalResult _ (Right (newAst, mem)) (Just o) =
    let (_, updatedMem) = translateToASM newAst mem
     in writeInstructionsToFile o updatedMem
            >> putStrLn ("Maryl ASM produced in " ++ o)
handleEvalResult _ (Right (newAst, mem)) Nothing =
    let (_, updatedMem) = translateToASM newAst mem
     in writeInstructionsToFile "out.masm" updatedMem
            >> putStrLn "Maryl ASM produced in out.masm"
handleEvalResult _ (Left err) _ = displayError err

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
handleImports' (x : xs) =
    readFile x >>= \content ->
        case parseAST content of
            Left err -> handleParseError True (Left err)
            Right asts ->
                handleImports asts >>= \handled ->
                    handleImports' xs >>= \next ->
                        return (handled ++ next)

handleImports :: [Ast] -> IO [Ast]
handleImports asts = case filter isImport asts of
    [] -> return $ filter (not . isImport) asts
    imports ->
        checkImports sImports
            >> handleImports' sImports
            <&> (\imported -> filter (not . isImport) imported ++ asts)
      where
        sImports = getImportFile <$> imports

checkImports :: [String] -> IO ()
checkImports [] = mempty
checkImports (f : fs)
    | isCorrectImport f = checkImports fs
    | otherwise = displayError $ "Incorrect import file \"" ++ f ++ "\""
  where
    isCorrectImport :: String -> Bool
    isCorrectImport "" = False
    isCorrectImport file
        | ".mrl" `isSuffixOf` file = True
        | otherwise = False

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

glados :: Mode -> IO ()
glados (Compile (Just path) out) = Monad.void (getContentFromFile initMemory path out)
glados (Compile Nothing _) = pError "No file provided"
glados (Vm path a) = vm path a
