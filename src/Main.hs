
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-matches #-}  -- TODO remove

module Main ( main ) where

import qualified Text.Megaparsec as P
import Control.Applicative
import Control.Monad.State
import Data.Array
import Data.Word
import Data.Char
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
  deriving (Show)

type Stream = String
type Error = Void
type Parser a = P.Parsec Error Stream a
type ParseError = P.ParseErrorBundle Stream Error

parse :: FilePath -> Stream -> Either ParseError [Instruction]
parse = P.parse parser

lexeme :: Parser a -> Parser a
lexeme p = p <* commentParser

parser :: Parser [Instruction]
parser = commentParser *> instructionsParser <* P.eof

instructionsParser :: Parser [Instruction]
instructionsParser = many $ lexeme instructionParser

instructionParser :: Parser Instruction
instructionParser
  =  increment
 <|> decrement
 <|> incrementPointer
 <|> decrementPointer
 <|> writeOutput
 <|> readInput
 <|> loop
  where
    c ==> i = i <$ P.single c
    increment = '+' ==> Increment
    decrement = '-' ==> Decrement
    incrementPointer = '>' ==> IncrementPointer
    decrementPointer = '<' ==> DecrementPointer
    writeOutput = '.' ==> WriteOutput
    readInput = ',' ==> ReadInput
    loop = Loop <$> P.between (lexeme $ P.single '[') (lexeme $ P.single ']') instructionsParser

commentParser :: Parser ()
commentParser = P.skipMany $ P.noneOf "+-><.,[]"

type Index = Int

data InterpreterState
  = InterpreterState
  { memory :: Array Index Word8
  , pointer :: Index
  }

type Interpreter a = StateT InterpreterState IO a

runInterpreter :: Interpreter a -> IO a
runInterpreter action = evalStateT action beginState
  where
    beginState = InterpreterState (listArray (0, 30000) [0, 0..]) 0

interpret :: [Instruction] -> Interpreter ()
interpret = traverse_ interpretSingle

interpretSingle :: Instruction -> Interpreter ()
interpretSingle = \case
  -- TODO handle boundary checks
  IncrementPointer -> modify $ \s -> s { pointer = pointer s + 1 }
  DecrementPointer -> modify $ \s -> s { pointer = pointer s - 1 }
  Increment -> do
    InterpreterState ram ptr <- get
    let value = ram ! ptr
        updatedRam = ram // [(ptr, value + 1)]
    modify $ \s -> s { memory = updatedRam }
  Decrement -> do
    InterpreterState ram ptr <- get
    let value = ram ! ptr
        updatedRam = ram // [(ptr, value - 1)]
    modify $ \s -> s { memory = updatedRam }
  WriteOutput -> do
    InterpreterState ram ptr <- get
    let value = ram ! ptr
        char = chr $ fromIntegral value
    liftIO . putChar $ char
  ReadInput -> do
    char <- liftIO getChar
    let byte = fromIntegral $ ord char
    InterpreterState ram ptr <- get
    let updatedRam = ram // [(ptr, byte)]
        updatedState = InterpreterState updatedRam ptr
    put updatedState
  Loop instructions -> do
    InterpreterState ram ptr <- get
    let value = ram ! ptr
    if value == 0
      then pure ()
      else do
        interpret instructions
        interpretSingle $ Loop instructions


main :: IO ()
main = do
  let filePath = "./src/beer.bf"
  contents <- readFile filePath
  case parse filePath contents of
    Left err -> putStrLn $ P.errorBundlePretty err
    Right instructions -> runInterpreter $ interpret instructions

