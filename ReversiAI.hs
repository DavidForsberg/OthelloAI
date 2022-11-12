-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI(State,author,nickname,initial,think) where

import Reversi
import Data.List(nub)
import Data.Maybe(isJust,isNothing)
import Control.Arrow(first)


-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{-
  This implementation contains a Othello (Reversi) system for communicating 
  with another Othello AI. For the AI part, the implementation use a normal 
  minimax algorithm, without Alpha-beta pruning. I chose a minimax maximum 
  recursion depth of 3. Which is foremost because the requirements was being
  able to run the complete game under 5 minutes. With depth of 4 this limit 
  was very closely broken. This is the reason I would have liked to go for 
  an Alpha-beta pruning approach which would have allowed a deeper search 
  in the game tree.

  Many functions require additional checking before being called, which would
  have required optimization, which I did not have time to finish. Some functions
  were also made as first templates to see if they worked, which I also did not 
  have the time to optimize. This affects the time it takes to recurse through
  minimax.   

  The evaluation function for each of the tree game leafs follows 
  four heuristics. These are immediate mobility, potential mobility,
  coin parity and corners. The choices behind using these heuristics 
  were mostly based upon an academic paper on Othello evaluation heuristics
  from two students at the University of Washington, available at:
  https://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf.

  Some of the basic data types for storing basic information is inspired by multiple 
  Stack Overflow and Github repositaries. One of which is available at:
  https://github.com/geon/Othello-in-Haskell/blob/master/othello.hs

  Another source of inspiration was the course Studium section on a Tic-Tac-Toe
  AI. Which inspired the pattern of the minimax function. 
 -}

-- the internal state of your AI

{- Represents an object which keeps track on the state of the AI.
   A tuple with the board and the AI:s colour. 
-}
type State = (Board, Reversi.Player)

author :: String
author = "David Forsberg Dimopoulos" 

nickname :: String
nickname = "ForsbergD"


{- Represents a square on the game board. 
   A tuple with the first integer value being the row (begins at 0) 
    and the second integer value being the column (begins at 0). 
-}
type Position = (Int, Int)

{- Represents a coin (marker/piece). 
   The Piece holds the position and the player at the given position.
-}
data Piece = Piece Position Reversi.Player deriving (Eq, Show)

{- Represents the game board (8x8).
   Contains a list of all placed pieces.
-}
type Board = [Piece]

{-
  The initial game board.
-}
initialBoard :: Board
initialBoard = 
  [
    Piece (3,4) Reversi.Black, 
    Piece (4,3) Reversi.Black,
    Piece (4,4) Reversi.White,
    Piece (3,3) Reversi.White
  ]

{-
  A list of all possible directions.
  Stored as a list of tuples with the same system as a Position.
-}
directions = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

{- Represents an heuristic evaluated value.
   Stores the value as a double. 
-}
type HeuristicValue = Double

{-
  Extracts the Integer value from a Move.
  PRE: Move cannot be Pass.
  RETURNS: The integer value of the move.
  EXAMPLES: moveToInt (Reversi.Move 4) -> 4
-}
moveToInt :: Reversi.Move -> Int
moveToInt (Reversi.Move int) = int

{-
  Converts a Reversi Move to a Piece.
  PRE: Move cannot be Pass.
  RETURNS: The piece located at the move (square).
  EXAMPLES: reversiMoveToField (Reversi.Move 42) (Reversi.Black) -> Piece (5,2) Black
-}
reversiMoveToField :: Reversi.Move -> Reversi.Player -> Piece
reversiMoveToField lastMove = Piece (moveToInt lastMove `quot` 8, moveToInt lastMove `mod` 8)

{-
  Converts a Reversi Move to a Piece.
  RETURNS: The move (square) located at the piece.
  EXAMPLES: fieldToReversiMove (Piece (0,0) Reversi.Black) -> Reversi.Move 0
-}
fieldToReversiMove :: Piece -> Reversi.Move
fieldToReversiMove (Piece (row, col) _) = Reversi.Move (8 * row + col)

{-
  Get opponent player.
  RETURNS: The opponent Reversi.Player to the passed player.
  EXAMPLES: getOpponent Reversi.Black -> Reversi.White
-}
getOpponent :: Reversi.Player -> Reversi.Player
getOpponent player 
  | player == Reversi.Black = Reversi.White 
  | otherwise = Reversi.Black

{-
  Get position of piece.
  RETURNS: The position of the piece.
  EXAMPLES: getPosition (Piece (2,3) Reversi.Black) -> (2,3)
-}
getPosition :: Piece -> Position
getPosition (Piece pos player) = pos 

{-
  Get empty adjecent fields to opponents coins, without duplicates
  (the potential moves for the passed player).
  RETURNS: The position of the piece.
  EXAMPLES: getPotentialMoves ([],Reversi.Black) -> []
            getPotentialMoves ([Piece (0,0) Reversi.White],Reversi.Black) -> [Piece (0,1) Black,Piece (1,1) Black,Piece (1,0) Black]
-}
getPotentialMoves :: State -> [Piece]
getPotentialMoves (board, player) = 
  let 
    oppMarkers = filter (\(Piece _ playerAtPos) -> playerAtPos /= player) board
    adjOppMarkers = concatMap findAdjMarkers oppMarkers
    emptyAdjMoves = filter (\(Piece pos _) -> isSquareEmpty pos board) adjOppMarkers
    adjMovesInbound = filter (\(Piece pos _) -> isSquareInbounds pos) emptyAdjMoves 
    noDuplicates = nub adjMovesInbound
  in 
    noDuplicates

{-
  Compares two heuristic values.
  RETURNS: An ordering which is either Equal, Greater than or Lesser than
  EXAMPLES: compareValue 2.2 1.0 -> GT
            compareValue 5.0 5.0 -> EQ
            compareValue 1.0 5.0 -> LT
-}
compareValue :: HeuristicValue -> HeuristicValue -> Ordering
compareValue x y | x == y = EQ
                 | x > y = GT
                 | otherwise = LT

{-
  Minimax algorithm for finding optimal move.
  PRE: There has to exist possible moves.
  RETURNS: A pass if its a leaf node and its heuristic value, otherwise, a piece and its heuristic value.
  EXAMPLE: minimax ([Piece (0,0) Reversi.Black,Piece (0,1) Reversi.White], Reversi.Black) 3 Reversi.Black (Reversi.Move (-1)) -> (Piece (0,2) Reversi.Black, *eval val*)
-}
-- VARIANT: depth and possible moves left in the tree.
minimax :: State -> Int -> Reversi.Player -> Reversi.Move -> (Maybe Piece, HeuristicValue)
minimax state@(currBoard, currPlayer) depth forsbergdPlayer lastMove = 
  let 
    {-
      Gets the Piece and Heuristic Value of the extremum the given value 
      at the given game board.
      PRE: The list cannot be empty.
      RETURNS: The tuple of the piece and the extremum value.
      EXAMPLES: extremum GT [(Piece (0,1) Reversi.Black, 10.0), (Piece (4,1) Reversi.Black, 5.0)] -> (Piece (0,1) Black,10.0)
    -}
    extremum :: Ordering -> [(Piece, HeuristicValue)] -> (Piece, HeuristicValue)
    extremum cmp =
      foldl1 (\(p1,v1) (p2,v2) ->
        if compareValue v1 v2 == cmp
          then (p1,v1)
          else (p2,v2))
    max = extremum GT
    min = extremum LT
    allMoves = findAllMoves state
  in 
    if null allMoves || depth == 0
      then (Nothing, eval (currBoard, forsbergdPlayer) (lastMove, getOpponent currPlayer))
      else 
        let 
          allMovesParsed = map fieldToReversiMove allMoves
          newObjects = map (\newMove -> (updateBoard newMove currPlayer state, newMove)) allMovesParsed 
          values = map snd (map (\((newBoard, currPlayer2), newMove) -> minimax (newBoard, getOpponent currPlayer2) (depth-1) forsbergdPlayer newMove) newObjects)
        in 
          first Just((if forsbergdPlayer == currPlayer then max else min) (zip allMoves values))

{-
  Evaluation based on heuristics applied on the game trees' leaf nodes.
  RETURNS: The heuristic value.
  EXAMPLES: eval (initial Reversi.White) ((Reversi.Move 19), Reversi.Black) -> 0.0
-}
eval :: State -> (Reversi.Move, Reversi.Player) -> HeuristicValue
eval (currBoard, currPlayer) (lastMove, lastPlayer) = 
  let
    -- Coin parity
    allSquares = length (filter (\(Piece pos player) -> player == currPlayer) currBoard)
    allOpponentSquares = length ( filter (\(Piece pos player) -> player /= currPlayer) currBoard)
    coinParity = 100 * fromIntegral (allSquares - allOpponentSquares) / fromIntegral(allSquares + allOpponentSquares)

    -- Immediate mobility
    allPossibleMoves = length ( findAllMoves (currBoard, currPlayer) )
    allOpponentPossibleMovesNext = length (findAllMoves (currBoard, getOpponent currPlayer))
    mobility = if allPossibleMoves - allOpponentPossibleMovesNext == 0
                  then 0
                  else 100 * fromIntegral (allPossibleMoves - allOpponentPossibleMovesNext) / fromIntegral (allPossibleMoves + allOpponentPossibleMovesNext)
    
    -- Potential mobility
    potentialMoves = length (getPotentialMoves (currBoard, currPlayer)) 
    opponentPotentialMoves = length (getPotentialMoves (currBoard, getOpponent currPlayer))
    potentialMobility = if potentialMoves - opponentPotentialMoves == 0
                          then 0
                          else 100 * fromIntegral (potentialMoves - opponentPotentialMoves) / fromIntegral (potentialMoves + opponentPotentialMoves)

    -- Corners
    topLeftCornerTaken = if any(\(Piece pos playerAtPos) -> pos == (0,0)) currBoard 
                          && getNextPieceColor (0,0) currBoard == currPlayer then 1 else 0
    topRightCornerTaken = if any(\(Piece pos playerAtPos) -> pos == (0,7)) currBoard 
                          && getNextPieceColor (0,7) currBoard == currPlayer then 1 else 0
    bottomLeftCornerTaken = if any(\(Piece pos playerAtPos) -> pos == (7,0)) currBoard 
                          && getNextPieceColor (7,0) currBoard == currPlayer then 1 else 0
    bottomRightCornerTaken = if any(\(Piece pos playerAtPos) -> pos == (7,7)) currBoard 
                          && getNextPieceColor (7,7) currBoard == currPlayer then 1 else 0

    topLeftCornerTakenOpp = if any(\(Piece pos playerAtPos) -> pos == (0,0)) currBoard 
                          && getNextPieceColor (0,0) currBoard /= currPlayer then 1 else 0
    topRightCornerTakenOpp = if any(\(Piece pos playerAtPos) -> pos == (0,7)) currBoard 
                          && getNextPieceColor (0,7) currBoard /= currPlayer then 1 else 0
    bottomLeftCornerTakenOpp = if any(\(Piece pos playerAtPos) -> pos == (7,0)) currBoard 
                          && getNextPieceColor (7,0) currBoard /= currPlayer then 1 else 0
    bottomRightCornerTakenOpp = if any(\(Piece pos playerAtPos) -> pos == (7,7)) currBoard 
                          && getNextPieceColor (7,7) currBoard /= currPlayer then 1 else 0

    sumCorners = topLeftCornerTaken + topRightCornerTaken + bottomLeftCornerTaken + bottomRightCornerTaken
    sumCornersOpponent = topLeftCornerTakenOpp + topRightCornerTakenOpp + bottomLeftCornerTakenOpp + bottomRightCornerTakenOpp
    cornersVal = if sumCorners - sumCornersOpponent == 0
                    then 0
                    else 100 * fromIntegral (sumCorners - sumCornersOpponent) / fromIntegral (sumCorners + sumCornersOpponent)
  in 
    coinParity + mobility + potentialMobility + cornersVal

{-
  Checks if a square is empty.
  RETURNS: True if square is empty, otherwise False.
  EXAMPLES: isSquareEmpty (0,0) initialBoard -> True 
            isSquareEmpty (4,3) initialBoard -> False
-}
isSquareEmpty :: Position -> Board -> Bool
isSquareEmpty wantedPos currBoard = not (any (\(Piece pos c) -> pos == wantedPos) currBoard)

{-
  Checks if a square (field) is on the board.
  RETURNS: True if position is inbounds, otherwise false.
  EXAMPLES: isSquareInbounds (0,0) -> True
            isSquareInbounds (-1,0) -> False

-}
isSquareInbounds :: Position -> Bool
isSquareInbounds (row,col) = (row >= 0 && row <= 7) && (col >= 0 && col <= 7)

{-
  Finds a Piece at the position a given direction.
  RETURNS: A piece.
  EXAMPLES: findFieldAt (Piece (0,0) Reversi.Black) (1,1) -> Piece (1,1) White
-}
findFieldAt :: Piece -> (Int, Int) -> Piece
findFieldAt (Piece (row, col) currPlayer) (rowAdd, colAdd) = Piece (row+rowAdd, col+colAdd) (getOpponent currPlayer)

{-
  Flips the marker/coin.
  RETURNS: A Piece.
  EXAMPLES: flipMarker (Piece (0,0) Reversi.Black) -> Piece (0,0) Reversi.White
-}
flipMarker :: Piece -> Piece
flipMarker (Piece pos player)
  | player == Black = Piece pos White
  | otherwise = Piece pos Black 

{-
  Finds adjecent markers, can be out of bounds and non-empty.
  RETURNS: A list of pieces.
  EXAMPLES: findAdjMarkers (Piece (0,0) Reversi.Black) -> [Piece (0,1) White,Piece (1,1) White,Piece (1,0) White,
                                                          Piece (1,-1) White,Piece (0,-1) White,Piece (-1,-1) White,
                                                          Piece (-1,0) White,Piece (-1,1) White]
-}
findAdjMarkers :: Piece -> [Piece]
findAdjMarkers piece = map (findFieldAt piece) directions 

{-
  Find the marker (coin) at the given position.
  RETURNS: The piece located at the position, or Nothing.
  EXAMPLES: findMarkerAtPos (4,4) (initial Reversi.Black) -> Just (Piece (4,4) White)
            findMarkerAtPos (0,0) (initial Reversi.Black) -> Nothing
-}
findMarkerAtPos :: Position -> State -> Maybe Piece
findMarkerAtPos posToCheck (currBoard, currPlayer) = 
  let 
    markersAtPos = filter(\(Piece pos _) -> pos == posToCheck) currBoard
  in 
    if markersAtPos /= []
      then Just (head markersAtPos)
      else Nothing

{-
  Gets the player's color at the position.
  PRE: There has to be a piece placed at the position.
  RETURNS: The color (player) which holds the piece at the position.
  EXAMPLES: getNextPieceColor (3,4) (initialBoard) -> Reversi.Black
-}
getNextPieceColor :: Position -> Board -> Reversi.Player 
getNextPieceColor nextPos currBoard = 
  let 
    (Piece _ foundColor) = head (filter(\(Piece pos _) -> pos == nextPos) currBoard)
  in 
    foundColor
  
{-
  Helper function for markersInRow'.
  RETURNS: A list of pieces.
  EXAMPLES: markersInRow (Piece (2,3) Reversi.Black) (initial Reversi.Black) (1,0) [] 0 -> [Piece (2,3) Black,Piece (3,3) White]
-}
markersInRow :: Piece -> State -> (Int, Int) -> [Piece] -> Int -> [Piece]
markersInRow piece = markersInRow' (Just piece)

{-
  Recursively find the pieces which makes a line in the given direction.
  RETURNS: List of markers that can be flipped, new piece included.
  EXAMPLES: markersInRow' Just (Piece (2,3) Reversi.Black) (initial Reversi.Black) (1,0) [] 0 -> [Piece (2,3) Black,Piece (3,3) White]
-}
-- VARIANT: The length (number of pieces) which makes a legal line.
markersInRow' :: Maybe Piece -> State -> (Int, Int) -> [Piece] -> Int -> [Piece]
markersInRow' Nothing _ _ _ 0 = [] 
markersInRow' Nothing _ _ _ largerThan0 = []
markersInRow' piece@(Just(Piece pos@(row, col) playerAtPos)) state@(currBoard, currPlayer) dir@(rowAdd, colAdd) totalList acc
  | isSquareEmpty (row, col) currBoard && isJust(findMarkerAtPos (row+rowAdd, col+colAdd) state) && getNextPieceColor (row+rowAdd, col+colAdd) currBoard == currPlayer = []
  | isNothing(findMarkerAtPos (row+rowAdd, col+colAdd) state) && acc == 0 = []
  | isSquareEmpty (row, col) currBoard && acc > 0 = []
  | isJust(findMarkerAtPos (row, col) state) && currPlayer == playerAtPos && acc > 0 = totalList
  | otherwise = markersInRow' (findMarkerAtPos (row+rowAdd, col+colAdd) state) state dir (totalList++[Piece pos playerAtPos]) (acc+1)

{-
  Get markers being flipped by making placing a coin, and the new placed piece.
  RETURNS: List of pieces being flipped.
  EXAMPLES: getFlippedMarkers (Piece (2,3) Reversi.Black) (initial Reversi.Black) -> [Piece (2,3) Black,Piece (3,3) White]
-}
getFlippedMarkers :: Piece -> State -> [Piece]
getFlippedMarkers playedMove state =
  let 
    playedMoveList = [playedMove]

    legalMoves = map (\piece -> markersInRow piece state (1,0) [] 0) playedMoveList ++
                 map (\piece -> markersInRow piece state (-1,0) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (0,1) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (0,-1) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (-1,-1) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (1,1) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (1,-1) [] 0) playedMoveList ++ 
                 map (\piece -> markersInRow piece state (-1,1) [] 0) playedMoveList 
                 

    legalMovesFiltered = concat( filter (/= []) legalMoves)
    noDuplicates = nub legalMovesFiltered
  in 
    noDuplicates

{-
  Find all possible moves given a board.
  RETURNS: List of possible pieces.
  EXAMPLES: findAllMoves (initial Reversi.Black) -> [Piece (2,3) Black,Piece (5,4) Black,Piece (3,2) Black,Piece (4,5) Black]
-}
findAllMoves :: State -> [Piece]
findAllMoves state@(currBoard, currPlayer) = 
  let 
    potentialMoves = getPotentialMoves state

    legalMoves = map (\piece -> markersInRow piece state (1,0) [] 0) potentialMoves ++
                 map (\piece -> markersInRow piece state (-1,0) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (0,1) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (0,-1) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (-1,-1) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (1,1) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (1,-1) [] 0) potentialMoves ++ 
                 map (\piece -> markersInRow piece state (-1,1) [] 0) potentialMoves
                 
    legalMovesFiltered = filter (/= []) legalMoves
    legalMovesComplete = map head legalMovesFiltered
  in 
    nub legalMovesComplete

{-
  Update board after a move is made.
  RETURNS: The updated state after making the move.
  EXAMPLES: updateBoard Pass Reversi.White (initial Reversi.Black) -> ([Piece (3,4) Black,Piece (4,3) Black,Piece (4,4) White,Piece (3,3) White],Black)
-}
updateBoard :: Reversi.Move -> Reversi.Player -> State -> State
updateBoard Reversi.Pass _ state = state
updateBoard lastMove player state@(currBoard,currPlayer) = 
  let 
    {-
      Remove piece from board.
      RETURNS: The board without the removed piece.
      EXAMPLES: removePiece (Piece (3,4) Reversi.Black) initialBoard -> [Piece (3,3) Reversi.White, Piece (4,3) Reversi.Black, Piece (4,4) Reversi.White]
    -}
    removePiece :: Piece -> [Piece] -> Bool
    removePiece (Piece oldPos _) newPieces = not (any (\(Piece pos c) -> oldPos == pos) newPieces)

    flippedMarkersRaw = getFlippedMarkers (reversiMoveToField lastMove player) (currBoard, player)
    flippedMarkers = head flippedMarkersRaw : map flipMarker (tail flippedMarkersRaw)
    boardWithoutPrev = filter (`removePiece` flippedMarkers) currBoard 

    newBoard = boardWithoutPrev ++ flippedMarkers
  in 
    (newBoard,currPlayer)

{-
  Create the initial state.
  RETURNS: A state with intial board and the passed player.
  EXAMPLES: initial (Reversi.Black) -> ([Piece (3,4) Black,Piece (4,3) Black,Piece (4,4) White,Piece (3,3) White], Black)
-}
initial :: Reversi.Player -> State
initial player = (initialBoard, player)

{-
  Use as the brain for the AI and is used for communicating with another AI.
  RETURNS: the optimal move with the updated board after making the move.
  EXAMPLES: think (initial Reversi.Black) Reversi.Pass 300 -> (Move 37,([Piece (3,4) Black,Piece (4,3) Black,Piece (3,3) White,Piece (4,5) Black,Piece (4,4) Black],Black))
-}
think :: State -> Reversi.Move -> Double -> (Reversi.Move,State)
think state@(_,currPlayer) lastMove timeLeft = 
  let 
    (boardAfterOpponent, _) = updateBoard lastMove (getOpponent currPlayer) state
    allMoves = findAllMoves (boardAfterOpponent, currPlayer)
  in 
    if null allMoves
      then (Pass, (boardAfterOpponent, currPlayer))
      else
        let
          (Just selectedMove, val) = minimax (boardAfterOpponent, currPlayer) 3 currPlayer (Reversi.Move (-1))
          convertedMove = fieldToReversiMove selectedMove
          (newBoard, _) = updateBoard convertedMove currPlayer (boardAfterOpponent, currPlayer) 
        in
          (convertedMove, (newBoard,currPlayer))