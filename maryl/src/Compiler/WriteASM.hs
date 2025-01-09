{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- WriteASM
-}

module Compiler.WriteASM (
    serializeInstructions,
    serializeFunction,
    serializeInstArgs,
    serializeInstruction,
    serializeMemoryFunctions,
    writeInstructionsToFile,
) where

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
serializeInstArgs (Push (L l)) = " " ++ show l
serializeInstArgs (Push (D d)) = " " ++ show d
serializeInstArgs (Push (Bi bi)) = " " ++ show bi
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
    let (serializedFuncs, _) = Map.foldlWithKey extractFunction ([], mem) mem
     in unlines serializedFuncs
  where
    extractFunction (acc, currentMem) key (AstDefineFunc val)
        | key /= "start" =
            let (instructions, updatedMem) = translateAST (AstDefineFunc val) currentMem
                serializedFunc = serializeFunction key instructions
             in (acc ++ [serializedFunc], updatedMem)
    extractFunction accAndMem _ _ = accAndMem -- != AstDefineFunc

serializeMain :: Memory -> (String, Memory)
serializeMain mem =
    let extractFunction (acc, currentMem) "start" ast =
            let (instructions, updatedMem) = translateAST ast currentMem
                serializedFunc = serializeFunction "start" instructions
             in (acc ++ [serializedFunc], updatedMem)
        extractFunction accAndMem _ _ = accAndMem
        (serializedMain, latestMem) = Map.foldlWithKey extractFunction ([], mem) mem
     in (unlines serializedMain, latestMem)
     
writeInstructionsToFile :: FilePath -> Memory -> IO ()
writeInstructionsToFile filePath mem =
    let (mainInstructions, updatedMem) = serializeMain mem
     in writeFile filePath (mainInstructions ++ serializeMemoryFunctions updatedMem)
