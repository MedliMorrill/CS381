module MiniLogo where

import Prelude hiding (Num)

-- Task 1
type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up
          | Down
  deriving (Eq,Show)

data Expr = Var
          | Num
          | Add Expr Expr
  deriving (Eq,Show)

-- data Cmd  = Pen Mode
--           | Move (Expr, Expr)
--           | Define macro ([Id]) {Prog}
--           | Call macro ([Expr])
--   deriving (Eq,Show)

data Cmd  = Pen Mode
          | Move Expr Expr
          | Define Macro [Var] Prog
          | Call Macro [Expr]
  deriving (Eq,Show)

-- Task 2
