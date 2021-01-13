
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day15" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input15.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 15a" $
                solvePart1 strs
                `shouldBe`
                18965440
            it "passes 15b" $
                solvePart2 strs
                `shouldBe`
                15862900


--                               Name   Cap Dur Flv Txt Cal
data IngredientInfo = Ingredient String Int Int Int Int Int deriving (Show, Eq)

data RecipeEntry = Entry IngredientInfo Int deriving (Show, Eq)

type Recipe = [RecipeEntry]

solvePart1 :: [String] -> Int
solvePart1 = maximum . map scoreInfoList . map recipeToInfoList . allRecipes . map parseIngredient

solvePart2 :: [String] -> Int
solvePart2 = maximum . map scoreInfoList . map recipeToInfoList . fiveHundredCalRecipes . allRecipes . map parseIngredient

parseIngredient :: String -> IngredientInfo
parseIngredient s = Ingredient name capacity durability flavor texture calories
    where
        parts = words s
        name = init $ head parts
        capacity = read $ init $ head $ drop 2 parts
        durability = read $ init $ head $ drop 4 parts
        flavor = read $ init $ head $ drop 6 parts
        texture = read $ init $ head $ drop 8 parts
        calories = read $ head $ drop 10 parts

allRecipes :: [IngredientInfo] -> [Recipe]
allRecipes = liftM2 map ingredientsOverPartition (flip partitions 100 . length)
--allRecipes ings = map (ingredientsOverPartition ings) $ partitions (length ings) 100

partitions :: Int -> Int -> [[Int]]
partitions 1 t = [[t]]
partitions n t = [ x : xs | x <- [0..t], xs <- partitions (n-1) $ t-x ]

ingredientsOverPartition :: [IngredientInfo] -> [Int] -> Recipe
ingredientsOverPartition = zipWith Entry


recipeToInfoList :: Recipe -> [[Int]]
recipeToInfoList = foldr (\(Entry (Ingredient _ c d f t _) amt) acc -> (map (* amt) [c, d, f, t]) : acc) []
--recipeToInfoList [] = []
--recipeToInfoList ((Entry (Ingredient _ c d f t _) amt) : xs) = (map (* amt) [c, d, f, t]) : recipeToInfoList xs

scoreInfoList :: [[Int]] -> Int
scoreInfoList = product . flip aux []
    where
        aux :: [[Int]] -> [Int] -> [Int]
        aux iList lst | concat iList == [] = lst
                      | otherwise =  aux rst $ (maxedSum : lst)
            where
                headRow = map head iList
                rst = map tail iList
                maxedSum = max 0 (sum headRow)


fiveHundredCalRecipes :: [Recipe] -> [Recipe]
fiveHundredCalRecipes = filter ((== 500) . calorieCount)

calorieCount :: Recipe -> Int
calorieCount = foldr (\(Entry (Ingredient _ _ _ _ _ cal) amt) acc -> acc + (cal * amt)) 0
