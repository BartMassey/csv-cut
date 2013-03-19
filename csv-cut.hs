-- Copyright © 2013 Bart Massey
-- Select columns and/or rows from a CSV file

import Data.List
import System.Console.ParseArgs
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.SSV

parseNumber :: Parser Int
parseNumber = fmap read (many1 digit)

parseRange :: Parser [Int]
parseRange = do
  maybeStart <- optionMaybe parseNumber
  let start = case maybeStart of
        Nothing -> 1
        Just n -> n
  _ <- char '-'
  maybeEnd <- optionMaybe parseNumber
  case maybeEnd of
    Nothing -> return [start - 1 ..]
    Just end -> return [start - 1 .. end - 1] 

parseRangeSpec :: Parser [Int]
parseRangeSpec = try parseRange <|> fmap ((:[]) . subtract 1) parseNumber

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge xl@(x : xs) yl@(y : ys)
  | x <= y = x : merge xs yl
  | otherwise = y : merge xl ys

parseSpec :: Parser [Int]
parseSpec = do
  ranges <- sepBy1 parseRangeSpec (char ',')
  return $ foldr1 merge ranges

readSpec :: String -> String -> [Int]
readSpec spec input =
  case " \t\n" `intersect` input of
    "" ->
      case parse parseSpec spec input of
        Left err -> error $ show err
        Right val -> val
    _ -> error $ "whitespace not allowed in " ++ spec

data ArgIndex = ArgCols | ArgRows | ArgSource deriving (Eq, Ord, Show)

argd :: [Arg ArgIndex]
argd = [ 
  Arg {
    argIndex = ArgCols,
    argAbbr = Just 'c',
    argName = Just "cols",
    argData = argDataOptional "column-spec" ArgtypeString,
    argDesc = "Expression describing columns to select." },
  Arg {
    argIndex = ArgRows,
    argAbbr = Just 'r',
    argName = Just "rows",
    argData = argDataOptional "row-spec" ArgtypeString,
    argDesc = "Expression describing rows to select." },
  Arg {
    argIndex = ArgSource,
    argAbbr = Nothing,
    argName = Nothing,
    argData = argDataOptional "csv-file" ArgtypeString,
    argDesc = "CSV file to process." } ]

select :: Int -> [Int] -> [[String]] -> [[String]]
select _ [] _ = []
select _ _ [] = []
select i (j : js) (s : ss) =
  case compare i j of
    GT -> select i js (s : ss)
    EQ -> s : select (i + 1) js ss
    LT -> select (i + 1) js ss

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let colspec = case getArg argv ArgCols of
        Nothing -> [0..]
        Just cs -> readSpec "column-spec" cs
  let rowspec = case getArg argv ArgRows of
        Nothing -> [0..]
        Just rs -> readSpec "row-spec" rs
  h <- getArgStdio argv ArgSource ReadMode
  text <- hGetContents h
  let csv = readCSV text
  let cols = select 0 colspec $ transpose csv
  let rows = select 0 rowspec $ transpose cols
  hPutCSV stdout rows
