{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- WriteASM
-}

module Compiler.WriteASM (serializeInstructions, serializeFunction, serializeInstArgs, serializeInstruction, serializeMemoryFunctions, writeInstructionsToFile) where

import Compiler.ASTtoASM (translateAST)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Memory (Memory)
import Parsing.ParserAst (Ast (..))
import VirtualMachine.Instructions (Inst (..), Instruction (..), Value (..))

serializeInstArgs :: Inst -> String
serializeInstArgs (Push (N n)) = " " ++ show n
serializeInstArgs (Push (B b)) = " " ++ show b
serializeInstArgs (Push (S s)) = " \"" ++ s ++ "\""
serializeInstArgs (Push (L s)) = " " ++ show s -- ? to check
serializeInstArgs (Push (D s)) = " " ++ show s
serializeInstArgs (Push (Bi s)) = " " ++ show s -- ? to check
serializeInstArgs (PushArg n) = " " ++ show n
serializeInstArgs (Call func) = " \"" ++ func ++ "\""
serializeInstArgs (Jump (Left n)) = " " ++ show n
serializeInstArgs (Jump (Right labelVal)) = " ." ++ labelVal
serializeInstArgs (JumpIfFalse (Left n)) = " " ++ show n
serializeInstArgs (JumpIfFalse (Right labelVal)) = " ." ++ labelVal
serializeInstArgs _ = ""

serializeInstruction :: Instruction -> String
serializeInstruction (Instruction _ nameVal instVal labelVal) =
    maybe "" (++ " ") labelVal ++ nameVal ++ serializeInstArgs instVal

serializeInstructions :: [Instruction] -> String
serializeInstructions = unlines . map serializeInstruction

serializeFunction :: String -> [Instruction] -> String
serializeFunction nameVal func = 
    -- trace (show nameVal) $
    "." ++ nameVal ++ " " ++ serializeInstructions func

serializeMemoryFunctions :: Memory -> String
serializeMemoryFunctions mem =
    unlines $ Map.foldrWithKey extractFunction [] mem
  where
    extractFunction key (AstDefineFunc val) acc 
        | key /= "start" =
            serializeFunction key (fst (translateAST (AstDefineFunc val) mem)) : acc 
        | otherwise = acc
    extractFunction _ _ acc = acc -- != AstDefineFunc

serializeMain :: Memory -> String
serializeMain mem = 
    unlines $ Map.foldrWithKey extractFunction [] mem
  where
    extractFunction "start" ast acc = serializeFunction "start" (fst (translateAST ast mem)) : acc
    extractFunction _ _ acc = acc -- != entrypoint

writeInstructionsToFile :: FilePath -> Memory -> IO ()
writeInstructionsToFile filePath mem =
    writeFile filePath (serializeMain mem ++ serializeMemoryFunctions mem)
