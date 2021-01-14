
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    text <- TIO.readFile "data/input17.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    print $ solvePart1 strs
    print $ solvePart2 strs

solvePart1 :: [String] -> Int
solvePart1 = length . filter (== 150) . map sum . subsequences . map read

solvePart2 :: [String] -> Int
solvePart2 = length . ap (filter . (==) . minimum . map length . filter ((150 ==) . sum) . subsequences . map read) (map length . filter ((150 ==) . sum) . subsequences . map read)
--solvePart2 s = length $ filter (== minNum) $ lengths
--    where
--        lengths = map length $ filter ((== 150) . sum) $ subsequences $ map read s
--        minNum = minimum $ map length $ filter ((== 150) . sum) $ subsequences $ map read s

