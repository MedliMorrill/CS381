--------------------------------------------------
----- Group Names: Andrew Morrill, Pierre-Louis Sixdenier, Ehmar Khan
----- Group ONIDs: morrilan,       sixdenip,               khaneh
----- Date: 3/01/18       			Class: CS 381
----- Main File Name: KarelSemantics.hs
----- Purpose: Semantics for Karel
----- Completion: 100% requirements done
--------------------------------------------------


module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = if test t w r then False else True
test (Facing cd) w (p, c, b) = getFacing(p, c, b) == cd 
test (Clear d) w r = isClear(relativePos d r) w 
test (Beeper) w (p, _, _) = hasBeeper p w
test (Empty) w (_, _, b) = if b == 0 then True else False 



-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r 

-- Move: if position is empty, move there.  Otherwise, error.
--   World doesnt change, robot position does
stmt Move _ w (p, c, b) = if test (Clear Front) w (p, c, b)
                            then OK w ((neighbor c p), c, b)
                            else Error ("Ran into a wall at: " ++ show (neighbor c p))

-- PickBeeper: if Beeper exists then pick up Beeper, otherwise Error
stmt PickBeeper _ w r = let q = getPos r
                        in if hasBeeper q w
                              then OK (decBeeper q w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show q)

-- PutBeeper: if Bag is empty then Error, otherwise place Beeper
stmt PutBeeper _ w r = let q = getPos r
                       in if isEmpty r
                             then Error ("No beeper to remove from bag") 
                             else OK (incBeeper q w) (decBag r)

-- Turn: turn and return OK, update Robot direction
--  No way for Turn to return Error 
stmt (Turn d) _ w (p, c, b) = OK w (p, (cardTurn d c), b) 

-- Block: Call self and send to other methods, end if error with a Error
--  or end with OK if no list.
stmt (Block []) def w r = OK w r
stmt (Block (i:j)) def w r = case stmt i def w r of
                               OK w' r' -> stmt (Block j) def w' r'
                               Error t  -> Error t
                               Done r -> Done r
-- If: calls stmt with Statement 1 if test evaluates to True,
--  or Statement 2 if False
stmt (If t s1 s2) def w r = if test t w r 
                               then stmt s1 def w r 
                               else stmt s2 def w r

-- Call: found function find, made my own version, findDef, to check for existing macro
--  If in the list, preform statment, otherwise return Error
stmt (Call m) def w r = case findDef m def of
                          Just s -> stmt s def w r 
                          Nothing -> Error("Undefined macro: " ++ m) 

-- Iterate: Preforms statement n times, returns OK at n = 0,
--  Recursive call Iterate if OK, Error if error occurs, Done r if shutdown, 
stmt (Iterate 0 s) def w r = OK w r
stmt (Iterate n s) def w r = case stmt s def w r of
                               OK w' r' -> stmt (Iterate (n - 1) s) def w' r'
                               Error t -> Error t
                               Done r -> Done r
-- While: Much like Iterate, except uses test, ending when
--  test returns False.  Everything else is the same.
stmt (While t s) def w r = if test t w r
                              then case stmt s def w r of
                                     OK w' r' -> stmt (While t s) def w' r'
                                     Error t -> Error t
                                     Done r -> Done r
                              else OK w r

findDef :: Macro -> Defs -> Maybe Stmt
findDef m [] = Nothing
findDef m ((n, s):j) = if m == n 
                          then Just s
                          else findDef m j
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r

