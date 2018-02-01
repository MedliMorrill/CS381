--------------------------------------------------
----- Group Names: Andrew Morrill, , 
----- Group ONIDs: morrilan      , ,
----- Date: 3/01/18       			Class: CS 381
----- Main File Name: MiniLogo.morrilan.hs
----- Purpose: Describe MiniLogo in Haskell
----- Completion:
--------------------------------------------------

module MiniLogo where

import Prelude hiding (Num)
import Data.List



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

--
-- define line(x1,y1,x2,y2) {
--   pen up; move(x1,y1);
--   pen down; move(x2,y2);
-- }

-- Assumes Pen is in down down to avoid repetition, so we lift Pen initially, then end it with the down position.

line = Define "line" ["x1","y1", "x2", "y2"] [Pen Up, Move (Ref "x1") (Ref "y1"), Pen Down, Move (Ref "x2") (Ref "y2")]




-- *** Task 3 *** --

-- 
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

pretty :: Prog -> String
pretty []       = []
pretty (x : xs) = stripAll x ++ pretty xs 



stripAll :: Cmd -> String
stripAll (Pen b)        = ("pen " ++ penCond b ++ "; ")
stripAll (Move e1 e2)   = ("move " ++ "("++  stripExpr e1 ++ ", "++ stripExpr e2 ++ "); ")
stripAll (Define m v p) = ("define " ++ m ++ " " ++ splitVar v ++ pretty p)
stripAll (Call m eL)    = ("call " ++ m ++ " (" ++ rmvChars 2 (splitExpr eL) ++ "); ")

penCond :: Mode -> String
penCond Up   = "up"
penCond Down = "down"

stripExpr :: Expr -> String
stripExpr (Ref v) = v
stripExpr (Lit n) = ("" ++ show n)
stripExpr (Add e1 e2) = (stripExpr e1) ++ " + " ++ (stripExpr e2)

splitVar :: [Var] -> String
splitVar []       = []
splitVar (v : vs) = v ++ " " ++ splitVar vs 

splitExpr :: [Expr] -> String
splitExpr []       = []
splitExpr (e : es) = stripExpr e ++ ", " ++ splitExpr es

-- Must have at least Int characters in the list
rmvChars :: Int -> [a] -> [a]
rmvChars 0 a = a
rmvChars n a = init (rmvChars (n - 1) a)

-- prog	::=	ε | cmd; prog

-- mode	::=	down | up

-- expr	::=	var	
--        | num	
--        | expr + expr

-- cmd	::=	pen mode	change pen mode
--        | move ( expr , expr )
--        | define macro ( var* ) { prog }
--        | call macro ( expr* )

-- *** TESTS *** --
-- 
-- >>> pretty [Pen Up, Move (Ref "x1") (Ref "y1"), Pen Down, Move (Ref "x2") (Ref "y2")]
-- 
-- >>> pretty [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]] 
-- 



