{-
-- EPITECH PROJECT, 2025
-- gladdos
-- File description:
-- Operators
-}

module VirtualMachine.Operators (operators) where

import VirtualMachine.Operators.Mathematicals (
    operatorAdd,
    operatorDiv,
    operatorMod,
    operatorMul,
    operatorSub,
 )

import VirtualMachine.Operators.Logical (
    logicalNot,
    operatorAnd,
    operatorEq,
    operatorGt,
    operatorLt,
    operatorNEq,
    operatorOr,
 )

import VirtualMachine.Operators.Binary (
    binaryAnd,
    binaryOr,
    binaryShiftL,
    binaryShiftR,
    binaryXor,
 )
import VirtualMachine.Operators.IO (
    opOpenFile,
    operatorAppendFile,
    operatorPrint,
    operatorReadFile,
    operatorWriteFile,
 )
import VirtualMachine.Operators.Lists (
    listPop,
    listPush,
    operatorGet,
    operatorSet,
 )
import VirtualMachine.State (V (Op))

operators :: [(String, V)]
operators =
    [ ("add", Op operatorAdd),
      ("sub", Op operatorSub),
      ("mul", Op operatorMul),
      ("div", Op operatorDiv),
      ("mod", Op operatorMod),
      ("eq", Op operatorEq),
      ("not", Op logicalNot),
      ("neq", Op operatorNEq),
      ("less", Op operatorLt),
      ("greater", Op operatorGt),
      ("and", Op operatorAnd),
      ("or", Op operatorOr),
      ("get", Op operatorGet),
      ("set", Op operatorSet),
      ("print", Op operatorPrint),
      ("readFile", Op operatorReadFile),
      ("writeFile", Op operatorWriteFile),
      ("appendFile", Op operatorAppendFile),
      ("band", Op binaryAnd),
      ("bor", Op binaryOr),
      ("xor", Op binaryXor),
      ("shiftR", Op binaryShiftR),
      ("shiftL", Op binaryShiftL),
      ("listPop", Op listPop),
      ("listPush", Op listPush),
      ("open", Op opOpenFile),
      ("close", Op opOpenFile)
    ]
