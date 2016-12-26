{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Program (Program, programFromText, testProgram, compile) where


import Prelude hiding (print)
import Evaluator
import Control.Monad.Trans.Writer
import Data.Functor.Identity

type Program = Writer Statement ()

instance Monoid Statement where
  mempty = Pass
  --mappend a b = do exec a >> exec Debug >> exec b
  mappend a b = (Debug a) `Seq` (Debug b)

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

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
                "arg"     .= int 10
                "scratch" .= var "arg"
                "total"   .= int 1
                while ( (var "scratch") `Gt` (int 1) ) (
                 do "total"   .=  "total" .* "scratch"
                    "scratch" .= "scratch" .- (1::Int)
                    print $ var "scratch"
                 )
                print $ var "total"
