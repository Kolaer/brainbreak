module Main where

import Lib

import qualified Data.Text.Lazy.IO as T

import LLVM.Pretty

main :: IO ()
main = do
  text <- getContents
  bbCode <- return $ parseBB text
  case bbCode of
    Left err -> putStrLn $ "Error: " ++ (show err)
    Right code -> do
      modul <- return $ generateModule code
      runJIT modul


-- main = T.putStrLn $ ppllvm $ generateModule
