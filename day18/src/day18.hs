
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Safe
import Data.Maybe

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    text <- TIO.readFile "data/input18.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    print $ solvePart1 strs
    print $ solvePart2 strs


solvePart1 :: [String] -> Int
solvePart1 = length . filter id . concat . head . drop 100 . iterate stepLights . parseToState

solvePart2 :: [String] -> Int
solvePart2 = length . filter id . concat . head . drop 100 . iterate (applyFaults . stepLights) . applyFaults . parseToState

type State = [Row]
type Row = [Bool]

parseToBool :: Char -> Bool
parseToBool '#' = True
parseToBool '.' = False

parseToRow :: String -> Row
parseToRow = map parseToBool

parseToState :: [String] -> State
parseToState = map parseToRow

stepLights :: State -> State
stepLights = flip (flip aux (0,0)) [[]]
    where
        aux ::  State -> (Int, Int) -> State -> State
        aux orig (row, col) new | row >= maxRow = init new
                                | col >= maxCol = aux orig (succ row, 0) (new ++ [[]])
                                | currentValue == Just True && (onNeighborCount `elem` [2,3]) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | currentValue == Just True = aux orig (row, succ col) (init new ++ [last new ++ [False]])
                                | currentValue == Just False && (onNeighborCount == 3) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | currentValue == Just False = aux orig (row, succ col) (init new ++ [last new ++ [False]])
            where
                maxRow = length orig
                maxCol = length $ head orig
                neighborLocations :: [(Int, Int)]
                neighborLocations = [(pred row, pred col), (pred row, col), (pred row, succ col),
                                             (row, pred col), (row, succ col),
                                              (succ row, pred col), (succ row, col), (succ row, succ col)]

                lookLoc :: State -> (Int, Int) -> Maybe Bool
                lookLoc state (r, c) = (state `atMay` r) >>= (`atMay` c)

                neighborValues :: [Bool]
                neighborValues =  map maybeBool $ map (lookLoc orig) neighborLocations

                currentValue = lookLoc orig (row, col)

                onNeighborCount = length $ filter id neighborValues

                maybeBool :: Maybe Bool -> Bool
                maybeBool (Just x) = x
                maybeBool Nothing = False



stepLightsB :: State -> State
stepLightsB = flip (flip aux (0,0)) [[]]
    where
        aux ::  State -> (Int, Int) -> State -> State
        aux orig (row, col) new | row >= maxRow = init new
                                | col >= maxCol = aux orig (succ row, 0) (new ++ [[]])
                                | currentValue == Just True && (onNeighborCount `elem` [2,3]) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | currentValue == Just True = aux orig (row, succ col) (init new ++ [last new ++ [False]])
                                | currentValue == Just False && (onNeighborCount == 3) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | currentValue == Just False = aux orig (row, succ col) (init new ++ [last new ++ [False]])
            where
                maxRow = length orig
                maxCol = length $ head orig
                neighborLocations :: [(Int, Int)]
                neighborLocations = [(pred row, pred col), (pred row, col), (pred row, succ col),
                                             (row, pred col), (row, succ col),
                                              (succ row, pred col), (succ row, col), (succ row, succ col)]

                lookLoc :: State -> (Int, Int) -> Maybe Bool
                lookLoc state (r, c) = (state `atMay` r) >>= (`atMay` c)

                neighborValues :: [Bool]
                neighborValues =  map maybeBool $ map (lookLoc orig) neighborLocations

                currentValue = lookLoc orig (row, col)

                onNeighborCount = length $ filter id neighborValues

                maybeBool :: Maybe Bool -> Bool
                maybeBool (Just x) = x
                maybeBool Nothing = False


applyFaults :: State -> State
applyFaults = flip (flip aux (0,0)) [[]]
    where
        aux :: State -> (Int, Int) -> State -> State
        aux orig (row, col) new | row >= maxRow = init new
                                | col >= maxCol = aux orig (succ row, 0) (new ++ [[]])| row == 0 && col == 0 = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | row == 0 && col == 0 = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | row == 0 && col == (pred maxCol) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | row == (pred maxRow) && col == 0 = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | row == (pred maxRow) && col == (pred maxCol) = aux orig (row, succ col) (init new ++ [last new ++ [True]])
                                | otherwise = aux orig (row, succ col) (init new ++ [last new ++ [fromJust currentValue]])

            where
                maxRow = length orig
                maxCol = length $ head orig
                lookLoc :: State -> (Int, Int) -> Maybe Bool
                lookLoc state (r, c) = (state `atMay` r) >>= (`atMay` c)
                currentValue = lookLoc orig (row, col)
