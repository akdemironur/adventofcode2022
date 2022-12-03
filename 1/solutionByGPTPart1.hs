import System.IO
import Data.List
import Data.Maybe

-- Read the contents of a text file and return it as a list of strings
readTextFile :: String -> IO [String]
readTextFile path = do
  -- Open the file in read-only mode
  handle <- openFile path ReadMode

  -- Read all the lines from the file
  content <- hGetContents handle

  -- Return the lines as a list of strings
  return (lines content)

-- Read a text file containing integers and empty lines, sum the integers
-- that are separated by blank lines, and return the maximum of those sums
maxIntSum :: String -> IO Int
maxIntSum path = do
  -- Read the lines of the file
  lines <- readTextFile path

  -- Split the lines into blocks separated by blank lines
  let blocks = groupBy (\x y -> (x /= "") && (y /= "")) lines

  -- Sum the integers in each block and return the maximum of those sums
  return (maximum (map (sum . map read . filter (not . null)) blocks))
