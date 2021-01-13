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
    spc <- spec
    tSpec <- testSpec "day12" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    str <- readFile "data/input12.txt"
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 12a" $
                solvePart1 str
                `shouldBe`
                Just 191164
            it "passes 12b" $
                solvePart2 str
                `shouldBe`
                Just 87842

-- lensing brought to you by https://www.reddit.com/r/adventofcode/comments/3wh73d/day_12_solutions/cxw8s38?utm_source=share&utm_medium=web2x&context=3
-- I needed to stumble through this until I got it to work, still need to learn about lenses
solvePart1 :: String -> Maybe Int
solvePart1 = toBoundedInteger . sumOf (cosmosOf plate . _Number) . (^?! _Value)

solvePart2 :: String -> Maybe Int
solvePart2 = toBoundedInteger . sumOf (cosmosOf (plate . filtered nonred) . _Number) . (^?! _Value)

nonred :: Value -> Bool
nonred (Object o) | (String $ T.pack "red") `elem` (M.elems o) = False
nonred _ = True
