module Types where

data BBCode =
  BBRight     -- >
  | BBLeft    -- <
  | BBInc     -- +
  | BBDec     -- -
  | BBOut     -- .
  | BBInput   -- ,
  | BBLoop [BBCode]
  deriving (Show, Eq)
