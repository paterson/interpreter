module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp         -- Abstractions
         | App Exp Exp          -- function applic ation
         deriving Show

data Value = IntVal Integer
           | FunVal Env Name Exp

type Env   = Map.Map Name Value -- Map names to values

eval0                  :: Env -> Exp -> Value
eval0 _ (Lit i)        = IntVal i
eval0 env (Var n)      = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = IntVal(i1 + i2)
                         where IntVal i1 = eval0 env e1
                               IntVal i2 = eval0 env e2
eval0 env (Abs n e)    = FunVal env n e
eval0 env (App e1 e2)  = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body


exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
eval0 Map.empty exampleExp

-- Convert to monad

type Eval1 a = Identity a          -- Use Identity monad as base monad

runEval1 :: Eval1 a -> a           -- For readability map runEval1 to runIdentity
runEval1 ev = runIdentity ev

-- Now rewrite eval0

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = Map.lookup n env                    -- This will trigger the containing monad's fail if it fails
eval1 env (Plus e1 e2) = do
                          IntVal i1 <- eval1 env e1
                          Intval i2 <- eval1 env e2
                          return $ IntVal(i1 + i2)
eval1 env (Abs n e)    = return $ FunVal env n e
eval1 env (App e1 e2)  = do
                          val1 <- eval1 env e1
                          val2 <- eval1 env e2
                          case val1 of
                               Funval env' n body -> eval1 (Map.insert n val2 env') body

-- Example
runEval1 $ eval1 Map.empty exampleExp         -- Run in identity monad

-- Add Error Handling

type eval2 a = ErrorT String Identity a      -- String = type of exceptions

runEval2 :: Eval2 a -> Either String a       -- Returns either a string or type a
runEval2 ev = runIdentity $ runErrorT ev

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = Map.lookup n env
eval2a env (Plus e1 e2) = do
                            IntVal i1 <- eval2a env e1
                            IntVal i2 <- eval2a env e2
                            return IntVal(i1 + i2)
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App e1 e2)  = do
                            val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                  FunVal env' n body -> eval2a (Map.insert n val2 env') body


runEval2 (eval2a Map.empty exampleExp)   -- Returns Right (IntVal 18)

-- But we can't catch errors with this, so..

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = case Map.lookup n env of
                            Nothing  -> throwError $ "Undeclared variable: " ++ n
                            Just val -> return val
eval2a env (Plus e1 e2) = do
                            i1 <- eval2a env e1
                            i2 <- eval2a env e2
                            in case (i1, i2) of
                                 (IntVal i1', Intval i2') -> return $ IntVal(i1' + i2')
                                 _                        -> throwError "Type mismatch"
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App e1 e2)  = do
                            val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            in case val1 of
                                  FunVal env' n body -> eval2a (Map.insert n val2 env') body
                                  _                  -> throwError "Type mismatch"

-- Now we get proper error messages:

runEval2 (eval2a Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))) -- Returns ⇒ Left "type mismatch"

-- Now let's try to hide the env variable since we're just passing it around
-- We can do that with the ReaderT monad

type Eval3 a = ReaderT Env (ErrorT String Identity) a   -- A reader monad passes a value into a computation and all its sub-computations.
                                                        -- This value can be read by all enclosed computations and get modified for nested computations

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runErrorT $ runReaderT ev env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = IntVal i
eval3 (Var n)      = do
                      env <- ask
                      case Map.lookup n env of
                        Nothing  -> throwError $ "Undeclared Variable: " ++ n
                        Just val -> return val
eval3 (Plus e1 e2) = do
                       e1' <- eval3 e1
                       e2' <- eval3 e2
                       case (e1', e2') of
                         (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                         _                      -> throwError "Type mismatch"
eval3 (Abs n e)    = do
                       env <- ask
                       return $ FunVal env n e
eval3 (App e1 e2)  = do
                       val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         -- local means we are going to modify the env' state inside, and const means we don't rely on the current value
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                         _                  -> throwError "Type mismatch"

-- Now we pass in the env at the start
runEval3 Map.empty (eval3 exampleExp)

-- Add State of type integer. Let's use it to track how 

type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)               -- Return two things:
runEval4 env st ev = runIdentity $ runStateT (runErrorT (runReaderT ev env)) st   --         - Either String or a
                                                                                  --         - State Integer

tick :: (Num s, MonadState s m) => m ()
tick = do
         st <- get
         put(st + 1)

eval4 :: Exp -> Eval3 Value
eval4 (Lit i)      = do
                      tick
                      return $ IntVal i
eval4 (Var n)      = do
                      tick
                      env <- ask
                      case Map.lookup n env of
                        Nothing  -> throwError $ "Undeclared Variable: " ++ n
                        Just val -> return val
eval4 (Plus e1 e2) = do
                       tick
                       e1' <- eval3 e1
                       e2' <- eval3 e2
                       case (e1', e2') of
                         (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                         _                      -> throwError "Type mismatch"
eval4 (Abs n e)    = do
                       tick
                       env <- ask
                       return $ FunVal env n e
eval4 (App e1 e2)  = do
                       tick
                       val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         -- local means we are going to modify the env' state inside, and const means we don't rely on the current value
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                         _                  -> throwError "Type mismatch"

-- Now we get back (Right (IntVal 18),8), which means there was 8 steps

-- Let us decide the return value for runEval4. Ignoring ReaderT as it does not affect the return value
-- (although it does affect runEval4’s arguments), runEval4 first peels off ErrorT and constructs a value of type Either String a.
-- Next, it peels off StateT and constructs a pair whose first component is the value being computed,
-- and whose second component is the side effect, i.e., the state.
-- Therefore, the type of the final result is (Either String a,Integer).
-- In contrast, runEval4′ first peels off StateT and then ErrorT. Hence we get Either String (a,Integer).

-- Adding Logging

type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

-- If you swaped the WriterT and ErrorT around, error messages would also get logged.

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $ runStateT (runWriterT (runErrorT (runReaderT ev env)) st

eval5 :: Exp -> Eval3 Value
eval5 (Lit i)      = do
                      tick
                      return $ IntVal i
eval5 (Var n)      = do
                      tick
                      tell [n]
                      env <- ask
                      case Map.lookup n env of
                        Nothing  -> throwError $ "Undeclared Variable: " ++ n
                        Just val -> return val
eval5 (Plus e1 e2) = do
                       tick
                       e1' <- eval3 e1
                       e2' <- eval3 e2
                       case (e1', e2') of
                         (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                         _                      -> throwError "Type mismatch"
eval5 (Abs n e)    = do
                       tick
                       env <- ask
                       return $ FunVal env n e
eval5 (App e1 e2)  = do
                       tick
                       val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         -- local means we are going to modify the env' state inside, and const means we don't rely on the current value
                         FunVal env' n body -> local $ const (Map.insert n val2 env') (eval3 body)
                         _                  -> throwError "Type mismatch"


-- Add I/O
-- It is not possible to define an I/O monad transformer, because the execution of I/O operations in Haskell
-- cannot be arbitrarily nested into other functions or monads, they are only allowed in the monad IO.

-- So we just use the IO Monad as our base instead..

type Eval6 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

-- In the eval6 function we can now use I/O operations, with one minor notational inconvenience:
-- we have to invoke the operations using the function liftIO, which lifts the I/O computation into the currently running monad.

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
                  tick
                  liftIO $ print i
                  return $ IntVal i
eval6 (Var n) =      do
                       tick
                       tell [n]
                       env <- ask
                       case Map.lookup n env of
                         Nothing  -> throwError $ "Undeclared variable: " ++ n
                         Just val -> return val
eval6 (Plus e1 e2) = do
                       tick
                       e1' <- eval6 e1
                       e2' <- eval6 e2
                       case (e1', e2') of
                         (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                         _                      -> throwError "Type mismatch"
eval6 (Abs n e)   = do
                      tick
                      env <- ask
                      return $ FunVal env n e
eval6 (App e1 e2) = do
                      tick
                      val1 <- eval6 e1
                      val2 <- eval6 e2
                      case val of
                        FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
