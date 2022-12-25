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
-- that are separated by blank lines, and return the sum of the three
-- largest sums of integers
maxIntSum :: String -> IO Int
maxIntSum path = do
  -- Read the lines of the file
  lines <- readTextFile path

  -- Split the lines into blocks separated by blank lines
  let blocks = groupBy (\x y -> (x /= "") && (y /= "")) lines

  -- Sort the blocks by the sum of their integers in descending order
  let sortedBlocks = sortBy (flip compare) (map (sum . map read . filter (not . null)) blocks)

  -- Return the sum of the three largest sums of integers
  return (sum (take 3 sortedBlocks))