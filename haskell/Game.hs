module Game where

import           Data.List

data Status = Dead | Alive deriving (Show, Eq)

type GridSize = (Int, Int)
type Grid = [[Status]]
type Cell = (Int, Int)

getGridSize :: Grid -> GridSize
getGridSize g = (length g, length (head g))

int2Status :: Int -> Status
int2Status 1 = Alive
int2Status 0 = Dead

createGrid :: [[Int]] -> Grid
createGrid = map (map int2Status)

getCellStatus :: Cell -> Grid -> Maybe Status
getCellStatus (x, y) g
  | x < 0 || y < 0 || x >= (length g) = Nothing
  | otherwise =
    let r = g !! x in
      if y >= (length r) then Nothing
      else Just (r !! y)

flipCellStatus :: Status -> Status
flipCellStatus Dead = Alive
flipCellStatus Alive = Dead

invertGrid :: Grid -> Grid
invertGrid = map (map flipCellStatus)

isValidCoordinate :: Cell -> Grid -> Bool
isValidCoordinate (x, y) g = x >= 0 && y >= 0 && x < szx && y < szy
                             where (szx, szy) = getGridSize g

getAliveNeighbourCount :: Cell -> Grid -> Int
getAliveNeighbourCount (x, y) g =
  let nbrs = map toStatus [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y  ),           (x+1, y  ),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]
             where toStatus = (\xy -> getCellStatus xy g) in
    foldl addNbrs 0 nbrs
    where addNbrs acc Nothing      = acc
          addNbrs acc (Just Dead)  = acc
          addNbrs acc (Just Alive) = acc + 1

getNextCellStatus :: Cell -> Grid -> Status
getNextCellStatus c g =
  let nbrs = getAliveNeighbourCount c g in
    case getCellStatus c g of
      Nothing    -> Dead -- won't happen with any luck
      Just Dead  -> if nbrs == 3 then Alive else Dead
      Just Alive -> if nbrs < 2 || nbrs > 3 then Dead else Alive

getNextGeneration :: Grid -> Grid
getNextGeneration g =
  toRows (map nextSt toCoords) []
  where
    nextSt c = getNextCellStatus c g
    (xsz, ysz) = getGridSize g
    toCoords = [(x, y) | x <- [0..(xsz - 1)], y <- [0..(ysz - 1)]]
    toRows [] g = g
    toRows ss g = toRows (drop xsz ss) ((take xsz ss) : g)

