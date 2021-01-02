import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.Split

import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day02" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input2.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 2a" $
                solvePart1 strs
                `shouldBe`
                1598415
            it "passes 2b" $
                solvePart2 strs
                `shouldBe`
                3812909


solvePart1 :: [String] -> Int
solvePart1 = sum . map (calcPaper . processDims)

solvePart2 :: [String] -> Int
solvePart2 = sum . map (calcRibbon . processDims)

processDims :: String -> [Int]
processDims = map read . splitOn "x"

calcPaper :: [Int] -> Int
calcPaper [l, w, h] = area + slack
    where
        lw = (l * w)
        hw = (w * h)
        lh = (l * h)
        slack = minimum [lw, hw, lh]
        area =  2*lw + 2*hw + 2*lh

calcRibbon :: [Int] -> Int
calcRibbon [l, w, h] = short + cube
    where
        cube = l * w * h
        lwP = 2 * l + 2 * w
        lhP = 2 * l + 2 * h
        hwP = 2 * w + 2 * h
        short = minimum [lwP, lhP, hwP]
