module Evaluator
  (Expr(..), Env, Eval, Statement(..), ProgramState(..), Name, Val(..),
   runEval, eval, currentEnv, lastStatement, initialProgramState
  ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Functor.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

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

data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
               | Debug Statement
               | Debug' Statement
               | DebugPrint Expr
               | DebugStepBack
               deriving (Show, Read, Eq)

data ProgramState = ProgramState { envs :: [Env], statements :: [Statement] }

currentEnv :: ProgramState -> Env
currentEnv ps = head $ envs ps

lastStatement :: ProgramState -> Statement
lastStatement ps = head $ statements ps

initialProgramState :: ProgramState
initialProgramState = ProgramState {envs=[Map.empty], statements=[]}
