import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day10" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input10.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 10a" $
                solvePart1 strs
                `shouldBe`
                252594
            it "passes 10b" $
                solvePart2 strs
                `shouldBe`
                3579328


-- This could be made more efficient by creating this list lazily and then using it in a more lazy way, but why bother for this case, it is fast enough
solvePart1 :: [String] -> Int
solvePart1 = length . (!! 40) . iterate speakNum . head

solvePart2 :: [String] -> Int
solvePart2 = length . (!! 50) . iterate speakNum . head

speakNum :: String -> String
speakNum = join . map (uncurry ((. return) . (++) . show)) . map (liftM2 (,) length head) . group
--speakNum = concat . map (\(c,e) -> (show c) ++ [e]) . map (\x -> (length x, head x)) . group


