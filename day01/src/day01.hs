import qualified Data.Text as T
import qualified Data.Text.IO as TIO


import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day01" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input1.txt"
    strs <- pure $ T.unpack text
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 1a" $
                solvePart1 strs
                `shouldBe`
                138
            it "passes 1b" $
                solvePart2 strs 1 0
                `shouldBe`
                1771



solvePart1 :: String -> Int
solvePart1 = sum . map translate
    where
        translate '(' = 1
        translate ')' = (-1)
        translate _ = error "Bad character"

solvePart2 :: String -> Int -> Int -> Int
solvePart2 [] _ _ = error "Did not reach basement"
solvePart2 _ pos (-1) = (pred pos)
solvePart2 (x:xs) pos n | x == '(' = solvePart2 xs (succ pos) (succ n)
                        | x == ')' = solvePart2 xs (succ pos) (pred n)
                        | otherwise = error "Bad character"
