-- DO NOT MODIFY THIS FILE

module Reversi(Player(Black,White), Move(Pass,Move)) where

{- The two players: Black and White. -}
data Player = Black | White
  deriving (Eq,Show)

{- Game moves.
   Pass represents a passing move. Move i represents a move to field i.
   INVARIANT: 0 <= i <= 63 in Move i
 -}
data Move = Pass | Move Int
  deriving (Eq,Show)
