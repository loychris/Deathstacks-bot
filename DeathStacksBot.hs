--- module (NICHT AENDERN!)
module DeathStacksBot where
--- imports (NICHT AENDERN!)
import Data.Char
import Util

--- external signatures (NICHT AENDERN!)



 
getMove :: String -> String
getMove command = showMove $ (getFilteredMoves command)!!0 

listMoves :: String -> String
listMoves command = foldr (++) [] $ map showMove $ getFilteredMoves command 


--- YOUR IMPLEMENTATION STARTS HERE ---


type Tower       = (String, Int, Int) 
type Board       = [Tower]
type Row         = (String, Int)
type Position    = (Int, Int)
type Direction   = (Int, Int)
type Move        = (Position, Int, Position)
type Destination = (Int, Int)


 



--------------------------- LIST MOVES 

-- takes in the initial command and returns a list of all possible moves for the next player 
getFilteredMoves :: String -> [Move]
getFilteredMoves command = removeDuplicates . filter filterNoChanges $ getAllMoves command 

-- takes in a List of moves and returns that list without duplicates in it
removeDuplicates :: [Move] -> [Move]
removeDuplicates moves = foldr funk [] moves

--takes in a move and a list of moves. If the list contains the move just the list gets returned. Othervise the list + move gets returned
funk :: Move -> [Move] -> [Move]
funk x moves = if elem x moves then moves else moves ++ [x]

-- takes in a move and returns true, if the move wouldn't change the board
filterNoChanges:: Move -> Bool
filterNoChanges (x, _, y) = if x == y then False else True

--takes in the command and returns all possible moves for the next player (may contain duplicates & noChangeMoves)
getAllMoves:: String -> [Move]
getAllMoves command = let towers = getTowers command 
                          movesLists = map getMovesForTower towers in
  foldr (++) [] movesLists






--------------------- SHOW MOVE

--toString(move)
showMove:: Move -> String
showMove (pos, steps, dest) = showPosition pos ++
  "-" ++ show steps ++
  "-" ++ showPosition dest

--toString(position)
showPosition:: Position -> String
showPosition (x,y) = let xStr = show (-x+6) in 
  case y of 0 -> "a" ++ xStr
            1 -> "b" ++ xStr
            2 -> "c" ++ xStr
            3 -> "d" ++ xStr
            4 -> "e" ++ xStr
            5 -> "f" ++ xStr

-- toString(tower)
showTower:: Tower -> String
showTower (str, x, y) = str ++ " " ++ showPosition (x,y)






--------------------------- GET MOVES FOR TOWER 

-- takes in a tower and returns a list of all possible moves for that tower
getMovesForTower:: Tower -> [Move]
getMovesForTower (towerString, x, y) = getMovesForTowerHelper (x,y) $ length towerString

getMovesForTowerHelper:: Position -> Int -> [Move]
getMovesForTowerHelper _ 0 = []
getMovesForTowerHelper pos i = getMovesForLength pos i ++ getMovesForTowerHelper pos (i-1) 

--takes in a position and a distance and returns all moves from that position with that distance 
getMovesForLength:: Position -> Int -> [Move]
getMovesForLength pos i = let destinations = getDestinations pos i in 
                          getMovesForLengthHelper pos i destinations []

getMovesForLengthHelper:: Position -> Int -> [Position] -> [Move] -> [Move]
getMovesForLengthHelper _ _ [] moves = moves 
getMovesForLengthHelper pos i (x:xs) moves = [(pos, i, x)] ++ getMovesForLengthHelper pos i xs moves
                          
--takes a position and a distance and returns a list of all Destinations
getDestinations:: Position -> Int -> [Destination]
getDestinations pos i = let directions = getDirections pos in
                        getDestinationsHelper pos i directions []

getDestinationsHelper:: Position -> Int -> [Direction] -> [Position]-> [Position]
getDestinationsHelper _ _ [] destinations = destinations
getDestinationsHelper pos i (x:xs) destinations = [(go i x pos)] ++ getDestinationsHelper pos i xs destinations






--------------------------- GET DIRECTIONS

-- takes in a position and returns all relevant directions [Direction]
getDirections :: Position -> [Direction]
getDirections pos = if onLeftEdge pos then 
                     if onTopEdge pos then [(1,0), (1,-1), (0,-1)] 
                     else if onBottomEdge pos then [(0,1), (1,0), (1,1)]
                      else [(0,1),(1,1),(1,0),(1,-1),(0,-1)]
                    else if onRightEdge pos then 
                     if onTopEdge pos then [(0,-1),(-1,-1),(-1,0)] 
                      else if onBottomEdge pos then [(-1,0),(-1,1),(0,1)]
                       else [(0,-1),(-1,-1),(-1,0),(-1,1),(0,1)]
                    else if onTopEdge pos then [(1,0),(1,-1),(0,-1),(-1,-1),(-1,0)]  
                     else if onBottomEdge pos then [(0,1),(1,1),(1,0),(-1,0),(-1,1)]
                    else allDirections


-- returns all possible directions
allDirections:: [Direction]
allDirections = [(0,1),(0,-1),(1,1),(-1,-1),(1,0),(-1,0),(1,-1),(-1,1)]






--------------------------- GO 

--takes in a number if steps, a direction and a position and returns the position the move lands on
go :: Int -> Direction -> Position -> Position
go 0     _           pos = pos
go steps (x,y) pos = if (onTopEdge pos) && (y > 0) then go steps (x, -y) pos else 
                           if (onLeftEdge pos) && (x < 0) then go steps (-x, y) pos else
                           if (onRightEdge pos) && (x > 0) then go steps (-x, y) pos else
                           if (onBottomEdge pos) && (y < 0) then go steps (x, -y) pos else  
                           go (steps-1) (x, y) (move (x, y) pos) 
                           
--takes in a direction and a position, goes one step in that direction and returns the new position
move:: Direction -> Position -> Position
move (dirX, dirY) (posX, posY) = (posX+dirX, posY+dirY) 

onLeftEdge :: Position -> Bool
onLeftEdge (0, _) = True
onLeftEdge _ = False

onRightEdge :: Position -> Bool
onRightEdge (5, _) = True
onRightEdge _ = False

onTopEdge :: Position -> Bool
onTopEdge (_, 5) = True
onTopEdge _ = False

onBottomEdge :: Position -> Bool
onBottomEdge (_, 0) = True
onBottomEdge _ = False






--------------------------- TOO TALL 

-- takes in a List of towers and returns true if any tower is higher than 4 
anyTooTall:: Board -> Bool
anyTooTall towers = any tooTall towers

--takes in a tower and returns true if the tower is too tall
tooTall:: Tower -> Bool
tooTall tower = if (getHeight tower) > 4 then True else False

--takes in a tower and returns its height
getHeight:: Tower -> Int
getHeight (towerString, _, _) = length towerString

--takes in a list of towers and returns the tallest one
getTallest :: Board -> Tower
getTallest towers = foldr higherTower ("", 0, 0) towers

--takes in two towers and returns the taller one
higherTower:: Tower -> Tower -> Tower
higherTower t1 t2 = if getHeight t1 >= getHeight t2 then t1 else t2






--------------------------- GET TOWERS

--takes in the command and returns the towers of the next player 
getTowers :: String -> Board
getTowers command = let towers = (getTowersNextPlayer command) in 
    if anyTooTall towers then [getTallest towers] else towers

--takes in the command and returns all Towers of the player whoes turn it is
getTowersNextPlayer :: String -> Board
getTowersNextPlayer command = let board = getBoard command in
    if redNext command then getRedTowers board else getBluTowers board

-- takes in a board and removes all towers, that aren't red
getRedTowers :: Board -> Board
getRedTowers board = filter isRed board

-- takes in a board and removes all towers, that aren't blue
getBluTowers :: Board -> Board
getBluTowers board = filter isBlu board

--takes in an tower and returns true if the top stone is red
isRed :: Tower -> Bool
isRed ((x:xs), _, _) = if x == 'r' then True else False

--tales in an tower and returns true if the top stone is blue 
isBlu :: Tower -> Bool
isBlu ((x:xs), _, _) = if x == 'b' then True else False

--takes in the command and returns true if its reds turn
redNext :: String -> Bool
redNext command = if last command == 'r' then True else False
--takes command and returns an list of all Towers on the board [(towerString, rowIndex, colIndex)]






--------------------------- GET BOARD 

-- takes in the command and returns all towers on the board
getBoard :: String -> Board
getBoard command = foldr parseRow [] (indexRows command)

-- takes in commandString, cuts off nextPlayer, returns boardString 
getBoardString :: String -> String
getBoardString s = (words s)!!0

--takes a function that chechs a condition for a char and a String and splits the string at every char that fulfills the condition
split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

--takes a String and a char, splits the String at char and returns a list of the substrings [String]
splitString :: Char -> String -> [String]
splitString _ [] = []
splitString x str  = (split (== x) str)

--takes command and returns List of [rowStrings]
getRowStrings :: String -> [String]
getRowStrings command = splitString '/' (getBoardString command)

-- takes command and returns list of rows [(rowString, rowIndex)]
indexRows :: String -> [Row]
indexRows command = indexRowsHelper (getRowStrings command) 0

indexRowsHelper :: [String] -> Int -> [Row]
indexRowsHelper [] _ = []
indexRowsHelper (x:xs) i =  [(x, i)] ++ (indexRowsHelper xs (i+1))  

-- takes a row(rowString, rowIndex) and list of Towers adds the towers in the row to the List and returns it 
parseRow :: Row -> Board -> Board
parseRow (rowString, rowIndex) x = x ++ parseRowHelper (rowString, rowIndex) 0

parseRowHelper :: (String, Int) -> Int -> [(String, Int, Int)]
parseRowHelper ([],_) _ = []
parseRowHelper (rowString, rowIndex) colIndex = if (rowString!!0 == ',') 
    then parseRowHelper ((drop 1 rowString), rowIndex) (colIndex+1) 
    else let currentTowerString = ((splitString ',' rowString)!!0) in
         [(currentTowerString, rowIndex, colIndex)] ++ parseRowHelper ((drop (length currentTowerString) rowString ), rowIndex) colIndex 