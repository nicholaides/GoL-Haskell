import Data.Set (Set)
import qualified Data.Set as Set

type Cell = (Integer,Integer)

data World = World { cells :: Set Cell, size :: (Integer, Integer) }


row :: y -> [x] -> [(x,y)]
row y xs = map (\x' -> (x', y) ) xs

rows :: [y] -> [x] -> [[(x,y)]]
rows ys xs = map (\y' -> row y' xs) ys

showRow :: Set Cell -> [Cell] -> String
showRow world_cells cells = map (\cell -> if Set.member cell world_cells then 'X' else ' ') cells

showRows :: Set Cell -> [[Cell]] -> String
showRows world_cells cell_matrix =
  unlines $ map (\cell_row ->
      showRow world_cells cell_row
    ) cell_matrix

instance Show World where
  show World { cells = c, size = (x,y) }=
    showRows c $ rows [0..y-1] [0..x-1]


world = World { cells = Set.fromList [ (0,0), (1,1), (0,1), (1, 0), (3,4), (2,4) ]
              , size  = (5, 5)
              }

main =
  putStrLn (show world)
