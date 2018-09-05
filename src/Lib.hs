{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Types
import Lexer
import Text.Megaparsec
import Data.Foldable
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.Operand

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction


parseBB = parse whileParser "repl"

bbCodeIR (BBRight, buffer, index, putchar, getchar) = do
  new_index <- add index (ConstantOperand $ C.Int 8 1)
  store index 4 new_index

bbCodeIR (BBLeft, buffer, index, putchar, getchar)  = do
  new_index <- sub index (ConstantOperand $ C.Int 8 1)
  store index 4 new_index

bbCodeIR (BBInc, buffer, index, putchar, getchar) = do
  addr <- add buffer index
  old_val <- load addr 4
  new_val <- add old_val (ConstantOperand $ C.Int 8 1)
  store addr 4 new_val

bbCodeIR (BBDec, buffer, index, putchar, getchar)   = do
  addr <- add buffer index
  old_val <- load addr 4
  new_val <- sub old_val (ConstantOperand $ C.Int 8 1)
  store addr 4 new_val

bbCodeIR (BBOut, buffer, index, putchar, getchar) = do
  addr <- add buffer index
  value <- load addr 4

  call putchar [(value, [])]

  return ()
-- bbCodeIR (BBInput, buffer, index) = undefined
-- bbCodeIR ((BBLoop code), buffer, index)) = undefined

codeIRTest align = do
  alloca AST.i8 Nothing align

generateModule code = buildModule "replModule" $ do
  putchar <- extern "putchar" [AST.i8] AST.i8
  getchar <- extern "getchar" [] AST.i8

  function "main" [] AST.i32 $ \[] -> do
    buffer <- alloca AST.i8 (Just $ ConstantOperand $ C.Int 32 30000) 4
    index <- alloca AST.i8 (Nothing) 4

    store index 4 (ConstantOperand $ C.Int 8 0)

    env <- return $ (map (\c -> (c, buffer, index, putchar, getchar)) code)

    forM_ env bbCodeIR

    ret $ ConstantOperand $ C.Int 32 0
