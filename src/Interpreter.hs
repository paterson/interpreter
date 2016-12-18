{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

module Interpreter
    (run, Program
    ) where

-- I want my own definition of lookup and I want to write my own function named "print".

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard print function using the name system.print

import qualified System.IO as System

-- Monads we'll use

import Data.Functor.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Control.Monad.Except

-- Expression language

data Val = I Int
         | B Bool
         deriving (Show, Read, Eq)

data Expr = Const Val
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | And Expr Expr
          | Or  Expr Expr
          | Not Expr
          | Eq  Expr Expr
          | Gt  Expr Expr
          | Lt  Expr Expr
          | Var String
          deriving (Show, Read, Eq)

data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
               deriving (Show, Read, Eq)

type Name = String
type Env = Map.Map Name Val

type Eval a = ReaderT Env (ExceptT String Identity) a

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> fail $ "Unknown variable " ++ k

runEval env expr = runIdentity $ runExceptT $ runReaderT expr env

evali op e0 e1 = do
                   e0' <- eval e0
                   e1' <- eval e1
                   case (e0', e1') of
                     (I i0, I i1) -> return $ I (i0 `op` i1)
                     _            -> fail "Type Error in arithmetic expression"

evalb op e0 e1 = do
                   e0' <- eval e0
                   e1' <- eval e1
                   case (e0', e1') of
                     (B i0, B i1) -> return $ B (i0 `op` i1)
                     _            -> fail "Type error in boolean expression"

evalib op e0 e1 = do
                    e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                      (I i0, I i1) -> return $ B (i0 `op` i1)
                      _            -> fail "Type error in arithmetic expression"

-- Evaluate

eval :: Expr -> Eval Val
eval (Const v)   = return v
eval (Add e0 e1) = do evali  (+)  e0 e1
eval (Sub e0 e1) = do evali  (-)  e0 e1
eval (Mul e0 e1) = do evali  (*)  e0 e1
eval (Div e0 e1) = do evali  div  e0 e1
eval (And e0 e1) = do evalb  (&&) e0 e1
eval (Or  e0 e1) = do evalb  (||) e0 e1
eval (Eq  e0 e1) = do evalib (==) e0 e1
eval (Gt  e0 e1) = do evalib (>)  e0 e1
eval (Lt  e0 e1) = do evalib (<)  e0 e1
eval (Not e0)    = do evalb  (const not) e0 (Const (B True))
eval (Var s)     = do env <- ask
                      lookup s env

data ProgramState = ProgramState { envs :: [Env] }

currentEnv :: ProgramState -> Env
currentEnv ps = head $ envs ps

initialProgramState :: ProgramState
initialProgramState = ProgramState {envs=[Map.empty]} 

type Run a = StateT ProgramState (ExceptT String IO) a
runRun p = runExcept (runStateT p initialProgramState)

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\ps ->
            let  updatedCurrentEnv = Map.insert s i (currentEnv ps)
                 tailEnvs          = tail $ envs ps
            in  ((), ProgramState{envs=updatedCurrentEnv:tailEnvs}))

exec :: Statement -> Run ()
exec (Assign s v)    = do
                         st <- get
                         Right val <- return $ runEval (currentEnv st) (eval v)
                         set (s,val)

exec (Seq s0 s1)     = do exec s0 >> exec s1

exec (Print e)       = do
                         st <- get
                         Right val <- return $ runEval (currentEnv st) (eval e)
                         liftIO $ System.print val
                         return ()

exec (If cond s0 s1) = do
                         st <- get
                         Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                         if val then do exec s0 else do exec s1

exec (While cond s)  = do
                         st <- get
                         Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                         if val then do exec s >> exec (While cond s) else return()

exec (Try s0 s1)     = do catchError (exec s0) (\e -> exec s1)

exec Pass            = return()


type Program = Writer Statement ()

instance Monoid Statement where
  mempty = Pass
  mappend a b = a `Seq` b

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

-- Executing a "program" means compiling it and then running the
-- resulting Statement with an empty variable map.

-- If this is step by step interactive we will probably instead be running exec nextStatement and passing in the current state
run :: Program -> IO ()
run p = do
          result <- runExceptT $ (runStateT (exec (compile p)) initialProgramState)
          case result of
            Right ((), _) -> return ()
            Left exn      -> System.print $ "Uncaught exception: " ++ exn


-- * Syntatic Sugar *

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
