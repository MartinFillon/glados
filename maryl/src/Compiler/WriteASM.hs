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

import Compiler.Translation.ASTtoASM (translateAST)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Memory (Memory)
import Parsing.ParserAst (Ast (..))
import VirtualMachine.Instructions (Inst (..), Instruction (..), Value (..))

serializeInstArgs :: Inst -> String
serializeInstArgs (Push val) = " " ++ serializeValue val
serializeInstArgs (PushArg n) = " " ++ show n
serializeInstArgs (Call func) = " \"" ++ func ++ "\""
serializeInstArgs (Jump (Left n)) = " " ++ show n
serializeInstArgs (Jump (Right labelVal)) = " ." ++ labelVal
serializeInstArgs (JumpIfFalse (Left n)) = " " ++ show n
serializeInstArgs (JumpIfFalse (Right labelVal)) = " ." ++ labelVal
serializeInstArgs (Load val) = " \"" ++ val ++ "\""
serializeInstArgs (Get val) = " \"" ++ val ++ "\""
serializeInstArgs _ = ""

serializeValue :: Value -> String
serializeValue (N n) = show n
serializeValue (B b) = show b
serializeValue (S s) = show s
serializeValue (L l) = show l
serializeValue (D d) = show d
serializeValue (C c) = "'" ++ [c] ++ "'"
serializeValue (Bi bi) = show bi
serializeValue (St s) = serializeStruct s

serializeStruct :: Map String Value -> String
serializeStruct struct =
    "{" ++ concatMapWithSeparator ", " serializeField (Map.toList struct) ++ "}"
  where
    serializeField (key, value) = "\"" ++ key ++ "\"=" ++ show value

concatMapWithSeparator :: String -> (a -> String) -> [a] -> String
concatMapWithSeparator sep f = foldr (\x acc -> f x ++ if null acc then "" else sep ++ acc) ""

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
    extractFunction (acc, currentMem) key (AstDefineFunc val) =
        let (instructions, updatedMem) = translateAST (AstDefineFunc val) currentMem
            serializedFunc = serializeFunction key instructions
         in (acc ++ [serializedFunc], updatedMem)
    extractFunction accAndMem _ _ = accAndMem -- != AstDefineFunc

writeInstructionsToFile :: FilePath -> Memory -> IO ()
writeInstructionsToFile filePath mem = writeFile filePath (serializeMemoryFunctions mem)
