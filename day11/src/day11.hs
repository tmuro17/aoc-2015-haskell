import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Monoid

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day11" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input11.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 11a" $
                solvePart1 strs
                `shouldBe`
                "vzbxxyzz"
            it "passes 11b" $
                solvePart2 strs
                `shouldBe`
                "vzcaabcc"


solvePart1 :: [String] -> String
solvePart1 = head . filter isLegalPassword . iterate incrementPass . head

solvePart2 :: [String] -> String
solvePart2 = head . tail . filter isLegalPassword . iterate incrementPass . head

incrementPass :: String -> String
incrementPass = reverse . aux . reverse
    where
        aux [] = []
        aux (x:xs) | (succ x) <= 'z' = (succ x):xs
                   | otherwise = 'a':(aux xs)

isLegalPassword :: String -> Bool
isLegalPassword = getAll . foldMap (All .) [hasThreeIncreasing, hasTwoRepeatingPairs, legalCharactersOnly]

hasThreeIncreasing :: String -> Bool
hasThreeIncreasing (x:y:z:rst) | (succ x) == y &&
                                 (succ y) == z = True
                               | otherwise = hasThreeIncreasing (y:z:rst)
hasThreeIncreasing _ = False

legalCharactersOnly :: String -> Bool
legalCharactersOnly = not . flip any "iol" . flip elem

hasTwoRepeatingPairs :: String -> Bool
hasTwoRepeatingPairs = (>=2) . length . nub . filter ((==2) . length) . group
