import Data.Array (Array)
import qualified Data.Array as Array

type Cell = (Integer,Integer)

data Row = Row [Bool]

type Dimension = (Int, Int)

lengthen n a0 list =
  let
    num_to_take = max 0 (n - length list)
    more = take num_to_take (repeat a0)
  in
    list ++ more

instance Show Row where
  show (Row bools) = let
    toChar = \b -> if b then 'X' else ' '
    in
      (map toChar bools) ++ "|\n"

parseRow :: String -> Row
parseRow s = Row $ map (=='X') s

data World = World Dimension [Row]
parseWorld :: String -> World
parseWorld s = let
    (str_x : str_y : body) = lines s
    x = read str_x
    y = read str_y
    rows  = map parseRow body
    long_rows = lengthen y (Row []) rows -- make sure we have enough rows. add extra rows if necessary
    all_long_rows = map (lengthen x False) (map (\(Row bools) -> bools) rows) -- make sure the rows are long enough. Fill them in w/ False if necessary
    real_rows = map Row all_long_rows
  in
    World (x, y) real_rows

instance Show World where
  show (World (x,y) rows) = let
      firstLine = (take x $ repeat '=') ++ ".\n"
      body = foldl1 (++) (map show rows)
    in
      firstLine ++ body

main = do
  worldStr <- getContents
  putStrLn $ show $ parseWorld worldStr
