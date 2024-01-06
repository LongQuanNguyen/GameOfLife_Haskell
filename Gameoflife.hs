import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.Console.ANSI

{- 
    Define Cell as a sum data type with two constructors Alive and Dead.
    Derriving Eq and Show type classes, Eq for equality and Show for printing.
    Grid is a list of lists of Cells.
-}
data Cell = Alive | Dead deriving (Eq, Show)
type Grid = [[Cell]]



{-
    Generate a random cell, import System.Random (randomRIO).
-}
randomCell :: IO Cell
randomCell = do
    cellState <- randomRIO (0, 1) :: IO Int
    return (if cellState == 0 then Dead else Alive)



{-
    Generate a random grid of given dimensions. 
    Import Control.Monad (replicateM) to call randomCell repeatedly.
-}
randomGrid :: Int -> Int -> IO Grid
randomGrid rows cols = replicateM rows (replicateM cols randomCell)



{-
    Take a cell coordinates and a grid. Return a list of all cell's neighbours.
    *** Make use of list comprehesion ***
-}
neighbours :: Grid -> (Int, Int) -> [Cell]
neighbours grid (x, y) = [grid !! x' !! y' | 
                        -- Get neighbours coordinates
                            x' <- [x-1..x+1],
                            y' <- [y-1..y+1], 
                            x' /= x || y' /= y, 
                        -- Check if neighbours are in the grid
                            x' >= 0, 
                            y' >= 0, 
                            x' < length grid, 
                            y' < length (head grid)]



{-
    Calculates the next state of a cell based on its current state and
    its neighbours.
    *** Make use of pattern matching ***
-}
nextCellState :: Cell -> [Cell] -> Cell
nextCellState Alive neighbours
    | aliveNeighbours < 2 = Dead
    | aliveNeighbours > 3 = Dead
    | otherwise = Alive
    where aliveNeighbours = length (filter (== Alive) neighbours)
nextCellState Dead neighbours
    | aliveNeighbours == 3 = Alive
    | otherwise = Dead
    where aliveNeighbours = length (filter (== Alive) neighbours)



{-
    Calulates the next state of the whole grid.
-}
aStep :: Grid -> Grid
aStep grid = [[nextCellState (grid !! x !! y) (neighbours grid (x, y)) 
                | y <- [0..cols-1]] | x <- [0..rows-1]]
    where   rows = length grid
            cols = length (head grid)



{-
    Run the Game of Life simulation. Proceed to next stage by pressing enter.
    Import System.Console.ANSI to clear the screen and set the cursor position.
-}
runSimulation :: Grid -> Int -> IO ()
runSimulation grid counter = do
    clearScreen
    setCursorPosition 0 0 -- Make the Grid stay in the same place in terminal
    putStrLn ""
    putStrLn $ "Current State: " ++ show counter
    putStrLn "--> Press enter to move on next state, Ctrl+C to exit <--"
    putStrLn ""
    putStrLn $ showGrid grid
    _ <- getLine -- Discard user input, waitng for Enter being pressed only
    runSimulation (aStep grid) (counter + 1)



{-
    Convert a grid to a string and print it.
    Again, awesome list comprehension.
-}
showGrid :: Grid -> String
showGrid grid = unlines [showRow row | row <- grid]
    where   showRow row = [showCell cell | cell <- row]
            showCell Alive = 'O'
            showCell Dead = ' '



main :: IO ()
main = do
    putStrLn "Enter the number of rows:"
    rows <- readLn
    putStrLn "Enter the number of columns:"
    cols <- readLn
    initialGrid <- randomGrid rows cols
    runSimulation initialGrid 0