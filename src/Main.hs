
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-matches #-}  -- TODO remove

module Main ( main ) where

import qualified Text.Megaparsec as P
import Control.Applicative
import Data.Void
import Data.Foldable


data Instruction
  = IncrementPointer   -- >
  | DecrementPointer   -- <
  | Increment          -- +
  | Decrement          -- -
  | WriteOutput        -- .
  | ReadInput          -- ,
  | Loop [Instruction] -- [ and ]
  | Nop                -- all the rest, TODO, remove from AST
  deriving (Show)

type Stream = String
type Error = Void
type Parser a = P.Parsec Error Stream a
type ParseError = P.ParseErrorBundle Stream Error

parse :: FilePath -> Stream -> Either ParseError [Instruction]
parse = P.parse parser

parser :: Parser [Instruction]
parser = do
  increments <- instructionsParser
  P.eof
  pure increments

instructionsParser :: Parser [Instruction]
instructionsParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser
  =  increment
 <|> decrement
 <|> incrementPointer
 <|> decrementPointer
 <|> writeOutput
 <|> readInput
 <|> loop
 <|> nop
  where
    c ==> i = i <$ P.single c
    increment = '+' ==> Increment
    decrement = '-' ==> Decrement
    incrementPointer = '>' ==> IncrementPointer
    decrementPointer = '<' ==> DecrementPointer
    writeOutput = '.' ==> WriteOutput
    readInput = ',' ==> ReadInput
    loop = Loop <$> P.between (P.single '[') (P.single ']') instructionsParser
    nop = Nop <$ P.noneOf "+-><.,[]"


type Interpreter = IO

interpret :: [Instruction] -> Interpreter ()
interpret = traverse_ interpretSingle

interpretSingle :: Instruction -> Interpreter ()
interpretSingle = \case
{-
  IncrementPointer -> _
  DecrementPointer -> _
  Increment -> _
  Decrement -> _
  WriteOutput -> _
  ReadInput -> _
  Loop instructions -> _
-}
  Nop -> pure ()
  _ -> print "for next time"


main :: IO ()
main = do
  let filePath = "./src/hello.bf"
  contents <- readFile filePath
  case parse filePath contents of
    Left err -> putStrLn $ P.errorBundlePretty err
    Right instructions -> interpret instructions

