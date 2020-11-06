module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7


--Added overloads below for mathematical expressions
--Overloads for extension have been commented out
--as they simplify the expression, thus making the diff
--function fail the test
instance Num Exp where
  fromInteger x = Val (fromIntegral x)
  -- negate 0      = Val 0
  negate x      = UnApp Neg x
  -- (+) 0 x       = x
  -- (+) x 0       = x
  (+) x y       = BinApp Add x y
  -- (*) 0 x       = Val 0
  -- (*) x 0       = Val 0
  -- (*) x 1       = x
  -- (*) 1 x       = x
  (*) x y       = BinApp Mul x y
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational x = Val (fromRational x)
  -- (/) 0 x       = 0
  -- (/) x 1       = x
  (/) x y        = BinApp Div x y
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin x     = UnApp Sin x
  cos x     = UnApp Cos x
  log x     = UnApp Log x
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

--Uses Haskell built-in function lookup to find value for key
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key table
  = fromJust (lookup key table)

--Function to convert the long expression into a readable one
--This is mainly for checking if expressions are correct when
--testing
showExp :: Exp -> String
showExp (BinApp op x1 x2)
  = ("(" ++ (showExp x1) ++ (getOp op) ++ (showExp x2) ++ ")")
  where
    getOp op
      | op == Add = "+"
      | op == Mul = "*"
      | op == Div = "/"

showExp (UnApp op x)
  = (getOp op) ++ "(" ++ (showExp x) ++ ")"
  where
    getOp op
      | op == Neg = "-"
      | op == Sin = "sin"
      | op == Cos = "cos"
      | op == Log = "log"

showExp (Val a)
  = show a

showExp (Id c)
  = c

--Tables below to be used for 'eval' function
--They map the mathematical expressions to the haskell functions
unOpMap = [(Neg, (*(-1))), (Sin, sin), (Cos, cos), (Log, log)]
binOpMap = [(Add, (+)), (Mul, (*)), (Div, (/))]

--eval function will take in a expression and and environment and
--calculate the result
eval :: Exp -> Env -> Double
eval (UnApp op x) en
  = (lookUp op unOpMap) (eval x en)

eval (BinApp op x1 x2) en
  = (lookUp op binOpMap) (eval x1 en) (eval x2 en)

eval (Val a) _
  = a

eval (Id c) en
  = lookUp c en

--diff function will take in an expression and calculate its
--derivative with respect to a given variable
diff :: Exp -> String -> Exp
diff (BinApp op x1 x2) v
  -- = (BinApp Add (BinApp Mul x1 (diff x2 v)) (BinApp Mul (diff x1 v) x2))
  | op == Add = (diff x1 v) + (diff x2 v)
  | op == Mul = (x1 * (diff x2 v)) + ((diff x1 v) * x2)
  | op == Div = (((diff x1 v) * x2) + (negate (x1 * (diff x2 v)))) / (x2 * x2)
  

diff (Val a) _
  = Val 0.0

diff (Id c) v
  | c == v = Val 1.0
  | otherwise = Val 0.0

diff (UnApp op x) v
  | op == Neg = negate (diff x v)
  | op == Sin = (cos x) * (diff x v)
  | op == Cos = negate ((sin x) * (diff x v))
  | op == Log = (diff x v) / x

--Redundant code
{-
diff :: Exp -> String -> Exp
diff (BinApp op x1 x2) v
  -- = (BinApp Add (BinApp Mul x1 (diff x2 v)) (BinApp Mul (diff x1 v) x2))
  | op == Add = BinApp Add (diff x1 v) (diff x2 v)
  | op == Mul = BinApp Add (BinApp Mul x1 (diff x2 v)) (BinApp Mul (diff x1 v) x2)
  | op == Div = BinApp Div (BinApp Add (BinApp Mul (diff x1 v) x2) (UnApp Neg (BinApp Mul x1 (diff x2 v)))) (BinApp Mul x2 x2)
  

diff (Val a) _
  = Val 0.0

diff (Id c) v
  | c == v = Val 1.0
  | otherwise = Val 0.0

diff (UnApp op x) v
  | op == Neg = UnApp Neg (diff x v)
  | op == Sin = BinApp Mul (UnApp Cos x) (diff x v)
  | op == Cos = UnApp Neg (BinApp Mul (UnApp Sin x) (diff x v))
  | op == Log = BinApp Div (diff x v) x
-}

--maclaurin function will perform the maclaurin expansion
--on a expression by repeated differentiation and substitution
maclaurin :: Exp -> Double -> Int -> Double
maclaurin ex p n
  = sum (zipWith3 (\x y z -> (x*z)/y) evs facs xp)
  where
    evs = map (`eval` [("x", 0.0)]) (take n (iterate (`diff` "x") ex))
    facs = 1 : take n (scanl (*) 1 [2..])
    xp = take n (iterate (*p) 1)

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
