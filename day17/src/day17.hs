
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day17" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input17.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 17a" $
                solvePart1 strs
                `shouldBe`
                4372
            it "passes 17b" $
                solvePart2 strs
                `shouldBe`
                4

solvePart1 :: [String] -> Int
solvePart1 = length . filter (== 150) . map sum . subsequences . map read

solvePart2 :: [String] -> Int
solvePart2 s = length $ filter (== minNum) $ lengths
    where
        lengths = map length $ filter ((== 150) . sum) $ subsequences $ map read s
        minNum = minimum $ lengths
-- Pointfree is cool but the above is lazier and therefore twice as fast
--solvePart2 = length . ap (filter . (==) . minimum . map length . filter ((150 ==) . sum) . subsequences . map read) (map length . filter ((150 ==) . sum) . subsequences . map read)

