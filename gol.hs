import Data.Array (Array)
import qualified Data.Array as Array

import Debug.Trace

type Cell = (Int,Int)

data Row = Row [Life]

type Dimension = (Int, Int)

data Life = Alive | Dead
  deriving Eq
fromBool b = if b then Alive else Dead

toChar Alive = 'X'
toChar _     = ' '

lengthen n a0 list =
  let
    num_to_take = max 0 (n - length list)
    more = take num_to_take (repeat a0)
  in
    list ++ more

instance Show Row where
  show (Row lifes) = (map toChar lifes) ++ "|\n"

parseRow :: String -> Row
parseRow s = Row $ map fromBool $ map (=='X') s

data World = World Dimension [Row]
parseWorld :: String -> World
parseWorld s = let
    (str_x : str_y : body) = lines s
    x = read str_x
    y = read str_y
    rows  = map parseRow body
    long_rows = lengthen y (Row []) rows -- make sure we have enough rows. add extra rows if necessary
    all_long_rows = map (lengthen x Dead) (map (\(Row lifes) -> lifes) rows) -- make sure the rows are long enough. Fill them in w/ Dead if necessary
    real_rows = map Row all_long_rows
  in
    World (x, y) real_rows

instance Show World where
  show (World (x,y) rows) = let
      firstLine = (take x $ repeat '=') ++ ".\n"
      body = foldl1 (++) (map show rows)
    in
      firstLine ++ body

cellLife :: World -> Cell -> Life
cellLife (World _ rows) (x,y) = let
    Row row = rows !! y
  in row !! x

possibleSurroundingCells :: Cell -> [Cell]
possibleSurroundingCells (x,y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0) ]

surroundingCells :: World -> Cell -> [Cell]
surroundingCells (World (bx,by) _) cell =
  filter (\(x,y) -> and [( x < bx ), ( y < by ), ( x >= 0 ), ( y >= 0 )]) $ possibleSurroundingCells cell

newState :: Life -> Int -> Life
newState Alive lifeCount
  | 2 == lifeCount = Alive
newState _ lifeCount
  | 3 == lifeCount = Alive

newState _ _       = Dead
getsLife :: World -> Cell ->  Life
getsLife world@(World (x,y) rows) cell = let
  life = cellLife world cell
  surroundingLife = filter (\l -> l == Alive) $ map (cellLife world) $ surroundingCells world cell
  in newState life (length surroundingLife)

tickRow :: World -> (Int, Row) -> Row
tickRow world (y, Row lifes) =
  Row $ map (\(x,life) -> getsLife world (x,y) ) (zip [0..] lifes)

tick :: World -> World
tick world@(World dim rows) = let
    newRows = map (tickRow world) (zip [0..] rows)
  in
    World dim newRows

main = do
  worldStr <- getContents
  let world = parseWorld worldStr
  putStrLn $ show $ world
  putStrLn $ show $ tick world
  putStrLn $ show $ tick $ tick world
  putStrLn $ show $ tick $ tick $ tick world
