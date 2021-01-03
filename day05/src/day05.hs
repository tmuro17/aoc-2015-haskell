import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Monoid
import Data.List (group, isInfixOf)

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day05" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input5.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 5a" $
                solvePart1 strs
                `shouldBe`
                238
            it "passes 5b" $
                solvePart2 strs
                `shouldBe`
                69

solvePart1 :: [String] -> Int
solvePart1 = length . filter isValidPart1

solvePart2 :: [String] -> Int
solvePart2 = length . filter isValidPart2

isValidPart1 :: String -> Bool
isValidPart1 str = getAll $ foldMap All [least3, doubleLetter, noBad]
    where
        least3 = (>= 3) $ length $ filter (`elem` "aeiou") str
        doubleLetter = (>= 2) $ maximum $ map length $ group str
        noBad = not $ any (flip isInfixOf str) ["ab", "cd", "pq", "xy"]



isValidPart2 :: String -> Bool
isValidPart2 str = getAll $ foldMap All [repeated, singleLetterGap]
    where
        singleLetterBetween :: String -> Bool
        singleLetterBetween [] = False
        singleLetterBetween (f:m:l:rest) | f == l = True
                                         | otherwise = singleLetterBetween (m:l:rest)
        singleLetterBetween _ = False

        repeatedPair :: String -> [String] -> Bool
        repeatedPair [] _ = False
        repeatedPair [x,y] seen = elem [x,y] seen
        repeatedPair (x:y:z:rst) seen | elem [x,y] seen = True
                                      | x == y && y == z = repeatedPair (z:rst) ([x,y]:seen)
                                      | otherwise = repeatedPair (y:z:rst) ([x,y]:seen)
        repeatedPair _ _ = False


        singleLetterGap = singleLetterBetween str
        repeated = repeatedPair str []



