{-
-- EPITECH PROJECT, 2024
-- maryl
-- File description:
-- VirtualMachine
-}

module VirtualMachine (vm) where

import Utils (handleParseError, pError)
import VirtualMachine.Parser (parseAssembly)

vm :: Maybe String -> IO ()
vm Nothing = pError "A file is required for the vm to run"
vm (Just s) = readFile s >>= (handleParseError True . parseAssembly) >>= print
