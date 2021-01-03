import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Hash.MD5
import Data.String.Utils (startswith)

import Test.Tasty
import Test.Tasty.Hspec



main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day04" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input4.txt"
    key <- pure $ T.unpack $ T.strip text
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 4a" $
                solvePart1 key
                `shouldBe`
                117946
            it "passes 4b" $
                solvePart2 key
                `shouldBe`
                3938038

solvePart1 :: String -> Int
solvePart1 = fst . head . filter (startswith "00000" . snd) . zip [1..] . flip map [1..] . hash

-- Takes about 30 seconds or so to run
solvePart2 :: String -> Int
solvePart2 = fst . head . filter (startswith "000000" . snd) . zip [1..] . flip map [1..] . hash

hash :: String -> Int -> String
hash k i = md5s $ Str $ k ++ (show i)
