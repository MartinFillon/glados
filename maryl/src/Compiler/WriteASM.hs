{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- WriteASM
-}

module Compiler.WriteASM () where

-- import VirtualMachine.Instructions (Inst(..), Value(..), Instruction(..))

-- serializeInstruction :: Instruction -> String
-- serializeInstruction (Instruction _ name inst label) =
--     maybe "" (++ ": ") label ++ name ++ serializeInstArgs inst

-- serializeInstArgs :: Inst -> String
-- serializeInstArgs (Push (N n)) = " " ++ show n
-- serializeInstArgs (Push (B b)) = " " ++ show b
-- serializeInstArgs (Push (S s)) = " \"" ++ s ++ "\""
-- serializeInstArgs (PushArg n) = " " ++ show n
-- serializeInstArgs (Call func) = " " ++ func
-- serializeInstArgs (Jump (Left n)) = " " ++ show n
-- serializeInstArgs (Jump (Right label)) = " " ++ label
-- serializeInstArgs (JumpIfFalse (Left n)) = " " ++ show n
-- serializeInstArgs (JumpIfFalse (Right label)) = " " ++ label
-- serializeInstArgs _ = ""

-- serializeInstructions :: [Instruction] -> String
-- serializeInstructions = unlines . map serializeInstruction

-- writeInstructionsToFile :: FilePath -> [Instruction] -> IO ()
-- writeInstructionsToFile filePath instructions = do
--     let serialized = serializeInstructions instructions
--     writeFile filePath serialized