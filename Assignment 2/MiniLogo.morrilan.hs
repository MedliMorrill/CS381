--------------------------------------------------
----- Group Names: Andrew Morrill, Pierre-Louis Sixdenier, Ehmar Khan
----- Group ONIDs: morrilan,       sixdenip,               khaneh
----- Date: 2/01/18       			Class: CS 381
----- Main File Name: MiniLogo.morrilan.hs
----- Purpose: Describe MiniLogo in Haskell
----- Completion: 100% requirements done, EC Task 7 seems to be unclear
-----   of how much simplification they want, Task 8 works for sure
----- Special Instructions: To run pretty, use putStrLn(pretty (p))
-----   p being the the Prog you want to pretty-print
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


-- Main method operated with putStrLn(pretty Prog)  Prog being the Prog wanting to prettify

pretty :: Prog -> String
pretty []       = []
pretty (x : xs) =  stripAll x ++ " \n" ++ pretty xs

-- Determines which Cmd is being selected, does the main text formatting, and calls the other helper functions.

stripAll :: Cmd -> String
stripAll (Pen b)        = ("pen " ++ penCond b ++ "; ")
stripAll (Move e1 e2)   = ("move " ++ "("++  stripExpr e1 ++ ", "++ stripExpr e2 ++ "); ")
stripAll (Define m v p) = ("define " ++ m ++ " (" ++ rmvChars 2 (splitVar v) ++ ") { \n" ++ rmvChars 1 (pretty p) ++ " \n}")
stripAll (Call m eL)    = ("call " ++ m ++ " (" ++ rmvChars 2 (splitExpr eL) ++ "); ")

-- Returns the seperate Pen conditions as a String

penCond :: Mode -> String
penCond Up   = "up"
penCond Down = "down"

-- Transforms Expr into a String

stripExpr :: Expr -> String
stripExpr (Ref v) = v
stripExpr (Lit n) = ("" ++ show n)
stripExpr (Add e1 e2) = (stripExpr e1) ++ " + " ++ (stripExpr e2)

-- Splits the Var list into a string

splitVar :: [Var] -> String
splitVar []       = []
splitVar (v : vs) = v ++ ", " ++ splitVar vs 

-- Splits the Expr list into a String, by sending each element to stripExpr

splitExpr :: [Expr] -> String
splitExpr []       = []
splitExpr (e : es) = stripExpr e ++ ", " ++ splitExpr es

-- Must have at least Int characters in the list, strips a certain amount of characters from the back
--  of a String

rmvChars :: Int -> [a] -> [a]
rmvChars 0 a = a
rmvChars n a = init (rmvChars (n - 1) a)


-- *** TESTS *** --

tp1 = putStrLn (pretty [Pen Up, Move (Ref "x1") (Ref "y1"), Pen Down, Move (Ref "x2") (Ref "y2")])
 
tp2 = pretty [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]] 

tp3 = pretty [Define "nix" ["x","y","w","h"] [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]]]



-- *** Task 7 *** --


--  Of what I know, this is as far I can simplify
--   Pulling things out would require complete resconstruction of expressions.
--   It works to one variable deep though.

optE :: Expr -> Expr
optE (Ref v) = (Ref v)
optE (Lit n) = (Lit n)
optE (Add e1 e2) = case (optE e1, optE e2) of
                     (Lit i, Lit j) -> Lit(i + j)
                     (Lit i, Add x (Lit j)) -> (Add (Lit(i + j)) x)
                     (Lit i, Add (Lit j) x) -> (Add (Lit(i + j)) x)
                     (Add (Lit i) x, Lit j) -> (Add (Lit(i + j)) x)
                     (Add x (Lit i), Lit j) -> (Add (Lit(i + j)) x)
                     _                      -> (Add e1 e2)

-- *** Tests *** --
-- optE (Add (Lit 8) (Add (Lit 2) (Ref "x"))))
-- optE (Add (Ref"x") (Add (Lit 2) (Lit 4))))
-- optE (Add (Add (Ref"x") (Add (Lit 2) (Lit 4))) (Lit 9))
-- optE (Add (Add (Lit 3) (Add (Lit 2) (Lit 4))) (Lit 9))




-- *** Task 8 *** --

optP :: Prog -> Prog
optP [] = []
optP (x : xs) = [findExpr x] ++ optP xs


findExpr :: Cmd -> Cmd
findExpr (Pen b)        = (Pen b)
findExpr (Move e1 e2)   = (Move (optE e1) (optE e2))
findExpr (Define m v p) = (Define m v (optP p))
findExpr (Call m eL)    = (Call m (simpleExpr eL))


simpleExpr :: [Expr] -> [Expr]
simpleExpr []       = []
simpleExpr (e : es) = [optE e] ++ simpleExpr es


-- *** Tests *** --

topt1 = optP [Call "line" [Ref "x", Ref "y", Add (Lit 1) (Lit 3), Add (Lit 4) (Ref "h")], Call "line" [Add (Lit 7) (Lit 8), Ref "y", Ref "x", Add (Lit 9) (Lit 10)]] 
