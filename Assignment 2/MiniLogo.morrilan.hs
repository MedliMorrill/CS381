module MiniLogo where

  import Prelude hiding (Num)
  
  -- Task 1
  type Num = Int
  type Var = [Char]
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
  
  -- Task 2
  
  
  -- Task 3

-- define nix (x,y,w,h) {
--   line(x,y,x+w,y+h);
--   line(x+w,y,x,y+h)
-- }

  nix = Define "nix" ["x","y","w","h"] [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x",Add (Ref "y") (Ref "h")]] 
  