module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


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
-- stmt Move _ _ _ = let
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r


w1 = const Nothing
w2 = const (Just 0)
r1 = ((0, 0), North, 0)

t1 = (test (Not(Facing North)) w1 r1)