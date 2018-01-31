--------------------------------------------------
----- Group Names: , , 
----- Group ONIDs: morrilan, ,
----- Date: 3/01/18       			Class: CS 381
----- Main File Name: MiniLogo.morrilan.hs
----- Purpose: Describe MiniLogo in Haskell
----- Completion:
--------------------------------------------------

module MiniLogo where

import Prelude hiding (Num)



-- *** Task 1 *** --

type Num   = Int
type Var   = [Char]
type Macro = String

type Prog = [Cmd]

data Mode = Up
          | Down
  deriving (Eq,Show)

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd  = Pen Mode
          | Move Expr Expr
          | Define Macro [Var] Prog
          | Call Macro [Expr]
  deriving (Eq,Show)


-- *** Task 2 *** --

-- define line(x1,y1,x2,y2) {
--   pen up; move(x1,y1);
--   pen down; move(x2,y2);
-- }

-- Assumes Pen is in down down to avoid repetition, so we lift Pen initially, then end it with the down position.

line = Define "line" ["x1","y1", "x2", "y2"] [Pen Up, Move (Ref "x1") (Ref "y1"), Pen Down, Move (Ref "x2") (Ref "y2")]



-- *** Task 3 *** --

-- define nix (x,y,w,h) {
--   line(x,y,x+w,y+h);
--   line(x+w,y,x,y+h);
-- }

-- Requires two calls to "line", first line starts from bottem left to the top right point of the X, 
--  second line starts from the bottom right to the top left of the X

nix = Define "nix" ["x","y","w","h"] [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]] 



-- *** Task 4 *** --

-- Draws from origin up one point then right, repeating it n times. 
--  Iterates n recursively, till it gets to 0, then adds a call and returns it back as a Prog
--  (added comments for variable check)     x1         y1         x2        y2                      x1         y1       x2     y2

steps :: Int -> Prog
steps 0 = []
steps n = steps (n-1) ++ [Call "line" [Lit (n-1), Lit (n-1), Lit (n-1), Lit (n)], Call "line" [Lit (n-1), Lit (n-1), Lit n, Lit n]]

-- steps :: Int -> Cmd
-- steps 0 = Call "line" [Lit 0, Lit 0, Lit 0, Lit 0]
-- steps n = Define "steps" ["x","y"] (iterateSteps n)



-- *** Task 5 *** --

-- prog to list of Cmds, then list of Cmds to list of names of Macros
-- macros recursively calls itself, through the list of Cmds entered, 
--  sends each one to stripName and returns the Macro names as a list

macros :: Prog -> [Macro]
macros [] = []
macros (x : xs) = (macros xs) ++ [stripName x]

-- takes Cmd from macros and strips the Macro name from the Call and Define

stripName :: Cmd -> Macro
stripName (Define m v p) = m
stripName (Call m e)     = m



-- *** Task 6 *** --