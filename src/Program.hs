{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Program (Program, compile, verify, programFromText, testProgram, failingProgram) where


import Prelude hiding (print)
import Evaluator
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.List

type Program = Writer Statement ()

instance Monoid Statement where
  mempty = Pass
  mappend a b = (Debug a) `Seq` (Debug b)

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

-- Detect unused variables
-- \\ gives the difference between two lists so I can remove all used variables using it.
verify :: Program -> [String]
verify p = verify' (compile p)  []

verify' :: Statement -> [String] -> [String]
verify' (Debug s         ) acc = verify' s acc
verify' (Seq s0 s1       ) acc = verify' s1 (verify' s0 acc)
verify' (Assign n e      ) acc = (n:acc) \\ (variablesInExpr e)
verify' (If e s0 s1      ) acc = verify' s1 (verify' s0 (acc \\ (variablesInExpr e)))
verify' (While e s0      ) acc = verify' s0 (acc \\ (variablesInExpr e))
verify' (Print e         ) acc = acc \\ (variablesInExpr e)
verify' (Try s0 s1       ) acc = verify' s1 (verify' s0 acc)
verify' _                  acc = acc

variablesInExpr :: Expr -> [String]
variablesInExpr (Var s    ) = [s]
variablesInExpr (Add e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Sub e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Mul e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Div e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (And e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Or  e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Eq  e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Gt  e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Lt  e0 e1) = variablesInExpr e0 ++ variablesInExpr e1
variablesInExpr (Not e0   ) = variablesInExpr e0
variablesInExpr _           = []

-- * Syntactic Sugar *
int = Const . I
bool = Const . B
var = Var

class SmartAssignment a where
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign v e = Assign v e

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr

instance PrettyExpr String String where
  x .* y = (Var x) `Mul` (Var y)
  x .- y = (Var x) `Sub` (Var y)

instance PrettyExpr String Int where
  x .* y = (Var x) `Mul` (Const (I y))
  x .- y = (Var x) `Sub` (Const (I y))

infixl 1 .=
(.=) :: String -> Expr -> Program 
var .= val = tell $ assign var val 

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

-- Helpers

programFromText :: String -> Program
programFromText s = read s

testProgram :: Program
testProgram = do
                "arg"       .= int 10
                "scratch"   .= var "arg"
                "total"     .= int 1
                while ( (var "scratch") `Gt` (int 1) ) (
                 do "total"   .=  "total" .* "scratch"
                    "scratch" .= "scratch" .- (1::Int)
                    print $ var "scratch"
                 )
                print $ var "total"

failingProgram :: Program
failingProgram = do
                "arg"       .= int 10
                "unusedvar" .= int 4
                "scratch"   .= var "arg"
                "total"     .= int 1
                while ( (var "scratch") `Gt` (int 1) ) (
                 do "total"   .=  "total" .* "scratch"
                    "scratch" .= "scratch" .- (1::Int)
                    print $ var "scratch"
                 )
                print $ var "total"
