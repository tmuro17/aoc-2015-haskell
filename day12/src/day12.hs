import qualified Data.Text as T

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Scientific
import qualified Data.HashMap.Strict as M

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    str <- readFile "data/input12.txt"
    print $ solvePart1 str
    print $ solvePart2 str

-- lensing brought to you by https://www.reddit.com/r/adventofcode/comments/3wh73d/day_12_solutions/cxw8s38?utm_source=share&utm_medium=web2x&context=3
-- I needed to stumble through this until I got it to work, still need to learn about lenses
solvePart1 :: String -> Maybe Int
solvePart1 = toBoundedInteger . sumOf (cosmosOf plate . _Number) . (^?! _Value)

solvePart2 :: String -> Maybe Int
solvePart2 = toBoundedInteger . sumOf (cosmosOf (plate . filtered nonred) . _Number) . (^?! _Value)

nonred :: Value -> Bool
nonred (Object o) | (String $ T.pack "red") `elem` (M.elems o) = False
nonred _ = True
