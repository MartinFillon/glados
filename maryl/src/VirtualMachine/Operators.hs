module VirtualMachine.Operators (operators) where

import VirtualMachine.Operators.Mathematicals (
    operatorAdd,
    operatorDiv,
    operatorMod,
    operatorMul,
    operatorSub,
 )

import VirtualMachine.Operators.Logical (
    operatorAnd,
    operatorEq,
    operatorGt,
    operatorLt,
    operatorNEq,
    operatorOr,
 )

import VirtualMachine.Operators.IO (operatorPrint)
import VirtualMachine.Operators.Lists (operatorGet, operatorSet)
import VirtualMachine.State (V (Op))

operators :: [(String, V)]
operators =
    [ ("add", Op operatorAdd),
      ("sub", Op operatorSub),
      ("mul", Op operatorMul),
      ("div", Op operatorDiv),
      ("mod", Op operatorMod),
      ("eq", Op operatorEq),
      ("neq", Op operatorNEq),
      ("less", Op operatorLt),
      ("greater", Op operatorGt),
      ("and", Op operatorAnd),
      ("or", Op operatorOr),
      ("get", Op operatorGet),
      ("set", Op operatorSet),
      ("print", Op operatorPrint)
    ]
