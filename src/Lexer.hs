module Lexer where
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Types


type Parser = Parsec Void String

sc = L.space ignored empty empty
  where ignored = skipSome $ satisfy $ (not . (`elem` "<>+-.,[]"))

symbol = L.symbol sc

bbright = symbol ">" >> return BBRight
bbleft  = symbol "<" >> return BBLeft
bbinc   = symbol "+" >> return BBInc
bbdec   = symbol "-" >> return BBDec
bbout   = symbol "." >> return BBOut
bbinput = symbol "," >> return BBInput
bbloop  = do
  _ <- symbol "["
  code <- many bbParser
  _ <- symbol "]"
  return $ BBLoop code

whileParser :: Parser [BBCode]
whileParser = between sc eof (many bbParser)

bbParser = bbright
  <|> bbleft
  <|> bbinc
  <|> bbdec
  <|> bbout
  <|> bbinput
  <|> bbloop
