-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Generate CSV files for testing.

import Data.List
import System.Environment
import System.IO (stdout)
import Text.SSV

digitWord :: Int -> String
digitWord i |i >= 0 && i <= 9 =
  ["zero", "one", "two", "three", "four", "five",
   "six", "seven", "eight", "nine"] !! i
digitWord _ =
  error "internal error: bad digit"

intWord :: Int -> String
intWord i | i < 0 =
  "minus " ++ intWord (0 - i)
intWord i =
  intercalate "-" $ map digitWord $ digits
  where
    digits = map ((read :: String -> Int) . (:"")) $ show i

genCSV :: Int -> Int -> [[String]]
genCSV nr nc =
  map genRow [1..nr]
  where
    genRow r =
      map ((intWord r ++) . ("/" ++) . intWord) [1..nc]

main :: IO ()
main = do
  [nr, nc] <- getArgs
  hPutCSV stdout $ genCSV (read nr) (read nc)
