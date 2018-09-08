{-# LANGUAGE RecursiveDo #-}
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
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.Analysis as LLysis
import LLVM.ExecutionEngine 
import LLVM.Module
import LLVM.Context
import LLVM.Exception
import LLVM.PassManager

import Foreign.Ptr (FunPtr, castFunPtr)

import Control.Monad

parseBB = parse whileParser "repl"

bbCodeIR (BBRight, buffer, index, putchar, getchar) = do
  old_index <- load index 4
  new_index <- add old_index (ConstantOperand $ C.Int 8 1)
  store index 4 new_index

bbCodeIR (BBLeft, buffer, index, putchar, getchar)  = do
  old_index <- load index 4
  new_index <- sub old_index (ConstantOperand $ C.Int 8 1)
  store index 4 new_index

bbCodeIR (BBInc, buffer, index, putchar, getchar) = do
  offset <- load index 4
  old_val_ptr <- gep buffer [offset]
  old_val <- load old_val_ptr 4
  new_val <- add old_val (ConstantOperand $ C.Int 8 1)
  store old_val_ptr 4 new_val
  return ()

bbCodeIR (BBDec, buffer, index, putchar, getchar) = do
  offset <- load index 4
  old_val_ptr <- gep buffer [offset]
  old_val <- load old_val_ptr 4
  new_val <- sub old_val (ConstantOperand $ C.Int 8 1)
  store old_val_ptr 4 new_val
  return ()

bbCodeIR (BBOut, buffer, index, putchar, getchar) = do
  offset <- load index 4
  value_ptr <- gep buffer [offset]

  value <- load value_ptr 4

  call putchar [(value, [])]

  return ()

bbCodeIR (BBInput, buffer, index, putchar, getchar) = do
  offset <- load index 4
  value_ptr <- gep buffer [offset]

  new_value <- call getchar []

  store value_ptr 4 new_value

  return ()

bbCodeIR ((BBLoop code), buffer, index, getchar, putchar) = mdo
  env <- return $ map (\c -> (c, buffer, index, getchar, putchar)) code

  br loop

  loop <- block `named` "loop"

  forM_ env bbCodeIR

  offset <- load index 4
  value_ptr <- gep buffer [offset]

  value <- load value_ptr 4

  cmp <- icmp IP.NE value (ConstantOperand $ C.Int 32 0)

  condBr cmp loop after

  after <- block `named` "after.loop"

  return ()

generateModule code = buildModule "replModule" $ do
  putchar <- extern "putchar" [AST.i32] AST.i32
  getchar <- extern "getchar" [] AST.i32

  function "main" [] AST.i32 $ \[] -> mdo
    block `named` "main"

    buffer <- alloca AST.i32 (Just $ ConstantOperand $ C.Int 32 3000) 4 

    index <- alloca AST.i32 (Nothing) 4

    store index 4 (ConstantOperand $ C.Int 32 0)

    br loop

    loop <- block `named` "init.loop"
    offset <- load index 4
    arr_ptr <- gep buffer [offset]
    store arr_ptr 4 (ConstantOperand $ C.Int 32 0)
    new_index <- add offset (ConstantOperand $ C.Int 32 1)
    cmp <- icmp IP.ULT new_index (ConstantOperand $ C.Int 32 3000)
    store index 4 new_index
    condBr cmp loop after
    after <- block `named` "init.loop.after"

    store index 4 (ConstantOperand $ C.Int 32 0)

    env <- return $ (map (\c -> (c, buffer, index, putchar, getchar)) code)

    forM_ env bbCodeIR

    ret $ ConstantOperand $ C.Int 32 0

passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

jit c = withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3
    model    = Nothing 
    ptrelim  = Nothing 
    fastins  = Nothing 

runJIT modul = do
  withContext $ \ctx -> do
    jit ctx $ \executionEngine ->
      withModuleFromAST ctx modul $ \m -> do
        withPassManager passes $ \pm -> do
          runPassManager pm m

          llir <- moduleLLVMAssembly m
          withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- maybe (return Nothing) (getFunction ee . mkName) (Just "main")
            maybe (return ()) (run >=> print . ("Exit code: " ++ ) . show)  mainfn

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))
