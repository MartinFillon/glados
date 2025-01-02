{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- WriteASM
-}

module Compiler.WriteASM () where

import VirtualMachine.Instructions (Inst(..), Value(..), Instruction(..))

serializeInstruction :: Instruction -> String
serializeInstruction (Instruction _ name inst label) =
    maybe "" (++ ": ") label ++ name ++ serializeInstArgs inst ++ "\n"

serializeInstArgs :: Inst -> String
serializeInstArgs (Push (N n)) = "push " ++ show n
serializeInstArgs (Push (B b)) = "push " ++ show b
serializeInstArgs (Push (S s)) = "push \"" ++ s ++ "\""
serializeInstArgs (Push (L s)) = "push " ++ show s -- ?
serializeInstArgs (Push (D s)) = "push " ++ show s
serializeInstArgs (Push (Bi s)) = "push " ++ show s -- ?
serializeInstArgs (PushArg n) = "pushArg " ++ show n
serializeInstArgs (Call func) = "call \" " ++ func ++ "\""
serializeInstArgs (Jump (Left n)) = "jump " ++ show n
serializeInstArgs (Jump (Right label)) = "jump ." ++ label
serializeInstArgs (JumpIfFalse (Left n)) = "jumpf " ++ show n
serializeInstArgs (JumpIfFalse (Right label)) = "jumpf ." ++ label
serializeInstArgs Noop = "noop"
serializeInstArgs Ret = "ret"
serializeInstArgs _ = ""

serializeInstructions :: [Instruction] -> String
serializeInstructions = unlines . map serializeInstruction

-- declare memory at start of asm with .<name> 

-- .start 
-- then rest of instructions of program

writeInstructionsToFile :: FilePath -> [Instruction] -> IO ()
writeInstructionsToFile filePath instructions = do
    let serialized = serializeInstructions instructions
    writeFile filePath serialized

    