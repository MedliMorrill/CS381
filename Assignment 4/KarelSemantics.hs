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


-- data Result = OK    World Robot
--             | Done  Robot
--             | Error String

-- Move: if position is empty, move there.  Otherwise, error.
--   World doesnt change, robot position does
-- PutBeeper: if bag is empty, error. otherwise remove beeper
-- Turn: turn and return OK, update Robot direction
-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r 
stmt Move _ w (p, c, b) = if test (Clear Front) w (p, c, b)
                            then OK w ((neighbor c p), c, b)
                            else Error ("Ran into a wall at: " ++ show p)
stmt PickBeeper _ w r = let q = getPos r
                        in if hasBeeper q w
                              then OK (decBeeper q w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show q)
stmt PutBeeper _ w r = let q = getPos r
                       in if isEmpty r
                             then Error ("No beeper to remove from bag") 
                             else OK (incBeeper q w) (decBag r)
stmt (Turn d) _ w (p, c, b) = OK w (p, (cardTurn d c), b) 
-- stmt (Turn d) _ w r = OK w (setFacing(cardTurn d (getFacing r))r)



-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r


w1 = const Nothing
w2 = const (Just 0)
r1 = ((0, 0), North, 0)

t1 = (test (Not(Facing North)) w1 r1)