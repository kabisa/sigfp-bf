
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-matches #-}  -- TODO remove

module Main ( main ) where

import qualified Text.Megaparsec as P
import Control.Applicative
import Control.Monad.State
import Data.Array.IO
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
type Memory = IOUArray Index Word8

data InterpreterState
  = InterpreterState
  { memory :: Memory
  , pointer :: Index
  }

type Interpreter a = StateT InterpreterState IO a

runInterpreter :: Interpreter a -> IO a
runInterpreter action = do
  ram <- newArray (0, 30000) 0
  let beginState = InterpreterState ram 0
  evalStateT action beginState

interpret :: [Instruction] -> Interpreter ()
interpret = traverse_ interpretSingle

interpretSingle :: Instruction -> Interpreter ()
interpretSingle = \case
  -- TODO handle boundary checks
  IncrementPointer -> modify $ \s -> s { pointer = pointer s + 1 }
  DecrementPointer -> modify $ \s -> s { pointer = pointer s - 1 }
  Increment -> do
    InterpreterState ram ptr <- get
    liftIO $ do
      currentValue <- readArray ram ptr
      writeArray ram ptr (currentValue + 1)
  Decrement -> do
    InterpreterState ram ptr <- get
    liftIO $ do
      currentValue <- readArray ram ptr
      writeArray ram ptr (currentValue - 1)
  WriteOutput -> do
    InterpreterState ram ptr <- get
    liftIO $ do
      value <- readArray ram ptr
      let char = chr $ fromIntegral value
      putChar char
  ReadInput -> do
    InterpreterState ram ptr <- get
    liftIO $ do
      char <- getChar
      let byte = fromIntegral $ ord char
      writeArray ram ptr byte
  Loop instructions -> do
    InterpreterState ram ptr <- get
    value <- liftIO $ readArray ram ptr
    when (value /= 0) $ do
      interpret instructions
      interpretSingle $ Loop instructions


-- TODO
-- 1. improve performance
--   - list of mutations per memory address
-- 2. error handling
-- 3. general refactorings

main :: IO ()
main = do
  let filePath = "./src/beer.bf"
  contents <- readFile filePath
  case parse filePath contents of
    Left err -> putStrLn $ P.errorBundlePretty err
    Right instructions -> runInterpreter $ interpret instructions

