import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day08" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input8.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 8a" $
                solvePart1 strs
                `shouldBe`
                1350
            it "passes 8b" $
                solvePart2 strs
                `shouldBe`
                2085

solvePart1 :: [String] -> Int
solvePart1 = ap ((-) . sum . map length) (sum . map (length . tail . init . decode))
--solvePart1 s = (sum $ map length s) - (sum $ map length $ map (tail . init) $ map decode s)

solvePart2 :: [String] -> Int
solvePart2 = ap ((-) . sum . map (length . encode)) (sum . map length)

-- credit https://www.reddit.com/user/haoformayor/
decode = f
 where f ('\\':'\\':xs)    = ('\\':decode xs)
       f ('\\':'"':xs)     = ('"':decode xs)
       f ('\\':'x':x:y:xs) = ('!':decode xs) -- Needed because of hex being weird
       f (x:xs)            = (x:decode xs)
       f []                = []

encode s = "\"" <> f s <> "\""
  where f ('"':xs)  = "\\\"" <> f xs
        f []        = []
        f ('\\':xs) = "\\\\" <> f xs
        f (x:xs)    = x:(f xs)

