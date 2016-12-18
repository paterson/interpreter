module Interpreter
    (run, programFromText, testProgram
    ) where

import Color
import Evaluator
import Program

import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System

-- Monads we'll use
import Data.Functor.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Control.Monad.Except

type Run a = StateT ProgramState (ExceptT String IO) a
runRun p = runExcept (runStateT p initialProgramState)

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\st ->
            let  updatedCurrentEnv = Map.insert s i (currentEnv st)
                 tailEnvs          = tail $ envs st
                 statements'       = statements st
            in  ((), ProgramState{envs=updatedCurrentEnv:tailEnvs, statements=statements'}))

exec                      :: Statement -> Run ()
exec (Assign s v)         = do st <- get
                               Right val <- return $ runEval (currentEnv st) (eval v)
                               set (s,val)
exec (Seq s0 s1)          = do exec s0 >> exec s1
exec (Print e)            = do st <- get
                               Right val <- return $ runEval (currentEnv st) (eval e)
                               liftIO $ System.print val
                               return ()
exec (If cond s0 s1)      = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec s0 else do exec s1
exec (While cond s)       = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec s >> exec (While cond s) else return()
exec (Try s0 s1)          = do catchError (exec s0) (\e -> exec s1)
exec Pass                 = return ()
exec (Debug s)            = do printNextStatement s
                               printCurrentVariables
                               printInstructions
                               exec (Debug' s)
                               liftIO $ putStrLn "Ticking.."
                               --tick s
exec (Debug' s)           = do input <- liftIO $ getLine
                               case input of
                                 "next" -> do exec s
                                              tick s
                                 "back" -> do exec DebugStepBack >> exec (Debug s)
                                 _      -> do exec (DebugPrint (Var input)) >> exec (Debug' s)
                               return ()
exec (DebugPrint (Var n)) = do st <- get
                               liftIO $ mapM_ putStrLn $ printVarHistory (envs st) n
                               return ()
exec DebugStepBack        = do st <- get
                               stepback
                               exec (Debug (lastStatement st)) -- st is old state so this is fine.

-- Executing a "program" means compiling it and then running the
-- resulting Statement with an empty variable map.
run :: Program -> IO ()
run p = do result <- runExceptT $ (runStateT (exec (compile p)) initialProgramState)
           case result of
             Right ((), _) -> return ()
             Left exn      -> System.print $ "Uncaught exception: " ++ exn

printVarHistory :: [Env] -> Name -> [String]
printVarHistory envs var = map (printVarAtEnv var) envs

printVarAtEnv :: Name -> Env -> String
printVarAtEnv var env = case Map.lookup var env of
                           Just val -> red $ "Value: " ++ (show val)
                           Nothing  -> red $ "Value: Not Set"

printNextStatement :: Statement -> Run ()
printNextStatement (While cond _) = liftIO $ putStrLn . yellow $ "Next Statement: While " ++ (show cond) ++ " { ... }"
printNextStatement (If cond _ _)  = liftIO $ putStrLn . yellow $ "Next Statement: If " ++ (show cond) ++ " { ... } Else { ... }"
printNextStatement s              = liftIO $ putStrLn . yellow $ "Next Statement: " ++ (show s)

printCurrentVariables = do st <- get
                           liftIO $ putStrLn . green $ "Current Variables: " ++ (show (currentEnv st))

printInstructions = liftIO $ putStrLn . green $ "Enter 'next' to proceed, 'back' to step back, 'skip' to skip and any variable name to view it's value"

tick :: Statement -> Run ()
tick s = do st <- get
            put $ ProgramState{ envs = (currentEnv st):(envs st), statements = s:(statements st) }
            return ()

stepback :: Run()
stepback = do st <- get
              put $ ProgramState{ envs = (tail $ envs st), statements = (tail $ statements st) }
              st' <- get
              return ()
