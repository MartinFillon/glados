{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- WriteASM
-}

module Compiler.WriteASM (serializeInstructions, serializeFunction, serializeInstArgs, serializeInstruction, serializeMemoryFunctions, writeInstructionsToFile) where

import Compiler.ASTtoASM (translateAST)
import qualified Data.Map as Map
import Memory (Memory)
import Parsing.ParserAst (Ast (..))
import VirtualMachine.Instructions (Inst (..), Instruction (..), Value (..))

serializeInstArgs :: Inst -> String
serializeInstArgs (Push (N n)) = " " ++ show n
serializeInstArgs (Push (B b)) = " " ++ show b
serializeInstArgs (Push (S s)) = " \"" ++ s ++ "\""
serializeInstArgs (Push (L s)) = " " ++ show s -- ?
serializeInstArgs (Push (D s)) = " " ++ show s
serializeInstArgs (Push (Bi s)) = " " ++ show s -- ?
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
    "." ++ nameVal ++ " " ++ serializeInstructions func

serializeMemoryFunctions :: Memory -> String
serializeMemoryFunctions mem =
    unlines $ Map.foldrWithKey extractFunction [] mem
  where
    extractFunction key (AstDefineFunc val) acc =
        serializeFunction key (translateAST (AstDefineFunc val)) : acc
    extractFunction _ _ acc = acc -- != AstDefineFunc

writeInstructionsToFile :: FilePath -> Memory -> [Instruction] -> IO ()
writeInstructionsToFile filePath mem instructions = do
    let functionDefinitions = serializeMemoryFunctions mem
    let serializedInstructions = serializeInstructions instructions
    if functionDefinitions /= ""
        then writeFile filePath (functionDefinitions ++ ".start " ++ serializedInstructions)
        else writeFile filePath serializedInstructions
