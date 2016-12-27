module Interpreter
    (run, programFromText, testProgram
    ) where

import Color
import Evaluator
import Program

import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System
import System.Console.ANSI
import System.Console.Regions
import Control.Concurrent.STM
import System.Console.Concurrent

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
                 outputs'          = outputs st
            in  ((), ProgramState{envs = updatedCurrentEnv : tailEnvs, statements = statements', outputs = outputs'}))

exec                      :: Statement -> Run ()
exec (Assign s v)         = do st <- get
                               Right val <- return $ runEval (currentEnv st) (eval v)
                               set (s,val)
exec (Seq s0 s1)          = do exec s0 >> exec s1
exec (Print e)            = do st <- get
                               Right val <- return $ runEval (currentEnv st) (eval e)
                               put $ ProgramState{ envs = (envs st), statements = (statements st), outputs = val:(outputs st) }
                               liftIO $ System.print val
                               return ()
exec (If cond s0 s1)      = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec (Debug s0) else do exec (Debug s1)
exec (While cond s)       = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec s >> exec DebugTidyTree >> exec (Debug (While cond s)) else return()
exec (Try s0 s1)          = do catchError (exec s0) (\_ -> exec s1)
exec Pass                 = return ()
exec (Debug (Seq s0 s1))  = exec (Seq s0 s1)                     -- Ignore this case as it's just chaining
exec (Debug s)            = do tick s
                               printTree s
                               printInstructions
                               exec (Debug' s)
exec (Debug' s)           = do input <- liftIO $ getLine
                               case input of
                                 "next" -> exec s
                                 "back" -> do exec DebugStepBack >> exec (Debug s)
                                 _      -> do exec (DebugPrint input) >> exec (Debug' s)
                               return ()
exec (DebugPrint n)       = do st <- get
                               liftIO $ mapM_ putStrLn $ printVarHistory (envs st) n
                               return ()
exec DebugStepBack        = do st <- get
                               let statement = lastStatement st
                               stepback
                               exec (Debug statement) -- st is old state so this is fine.
exec DebugTidyTree        = do st <- get
                               clearFromTree (lastStatement st)

-- Executing a "program" means compiling it and then running the
-- resulting Statement with an empty variable map.
run :: Program -> IO ()
run p = do result <- runExceptT $ (runStateT (exec (compile p)) initialProgramState)
           case result of
             Right ((), _) -> return ()
             Left exn      -> System.print $ "Uncaught exception: " ++ exn

tick :: Statement -> Run ()
tick s = do st <- get
            put $ ProgramState{ envs = (currentEnv st):(envs st), statements = s:(statements st), outputs = (outputs st) }

stepback :: Run ()
stepback = do st <- get
              put $ ProgramState{ envs = (tail $ envs st), statements = (tail $ (tail $ statements st)), outputs = (outputs st) }

clearFromTree :: Statement -> Run ()
clearFromTree s = do st <- get
                     put $ ProgramState{ envs = (envs st), statements = (tail $ statements st), outputs = (outputs st) }
                     case s of
                       (While _ _) -> return ()
                       _           -> clearFromTree $ lastStatement st

printVarHistory :: [Env] -> Name -> [String]
printVarHistory xs var = map (printVarAtEnv var) (reverse xs)

printVarAtEnv :: Name -> Env -> String
printVarAtEnv var env = case Map.lookup var env of
                           Just val -> red $ "Value: " ++ (show val)
                           Nothing  -> red $ "Value: Not Set"

printInstructions :: Run ()
printInstructions = liftIO $ putStrLn "Enter 'next' to proceed, 'back' to step back, 'skip' to skip and any variable name to view it's value"

-- Takes in next statement and prints it with the rest of the history tree
printTree :: Statement -> Run ()
printTree s = do st <- get
                 let statements' = tail $ statements st
                 let list = zipWithPadding Pass ("", Null) Null (reverse statements') (Map.toList (currentEnv st)) (reverse (outputs st))
                 clearTerminal
                 liftIO $ printTreeHeader
                 liftIO $ mapM_ (putStrLn . printTreeLine) $ list
                 liftIO $ putStrLn $ printCurrentStatement s

printTreeHeader :: IO ()
printTreeHeader = do liftIO $ putStrLn $ "File" ++ (nspaces (60-4)) ++ "Variables" ++ (nspaces (60-9)) ++ "Output"
                     liftIO $ putStrLn $ "====" ++ (nspaces (60-4)) ++ "=========" ++ (nspaces (60-9)) ++ "======"

printTreeLine :: (Statement, (Name, Val), Val) -> String
printTreeLine (s, var, val) = (cyan $ statementToString s) ++ spaces ++ printVariable var ++ spaces' ++ printOutput val
                  where spaces = nspaces (60 - l)
                        spaces' = nspaces (60 - l')
                        l  = length $ statementToString s
                        l' = length $ printVariable var

printCurrentStatement :: Statement -> String
printCurrentStatement s = green $ "> " ++ statementToString s

printVariable :: (Name, Val) -> String
printVariable (_, Null) = ""
printVariable (n, v)    = "Variable: " ++ (blue n) ++ " Value: " ++ (blue (show v))

printOutput :: Val -> String
printOutput Null = ""
printOutput s    = show s

-- Clear screen twice ensures we are at the bottom of the screen.
clearTerminal :: Run ()
clearTerminal = do liftIO $ clearScreen
                   liftIO $ clearScreen

statementToString :: Statement -> String
statementToString (While cond _) = "While " ++ (show cond) ++ " do"
statementToString (If cond _ _)  = "If " ++ (show cond) ++ " do"
statementToString Pass           = ""                                  -- Don't bother printing pass statements
statementToString s              = show s

-- Normalize's lengths of zipped lists (So will fill with standard defaults)
zipWithPadding :: a -> b -> c -> [a] -> [b] -> [c] -> [(a,b,c)]
zipWithPadding a b c (x:xs) (y:ys) (z:zs) = (x,y,z) : zipWithPadding a b c xs ys zs
zipWithPadding _ _ _ []     []     []     = []
zipWithPadding a b c []     ys     zs     = zipWithPadding a b c [a] ys zs
zipWithPadding a b c xs     []     zs     = zipWithPadding a b c xs [b] zs
zipWithPadding a b c xs     ys     []     = zipWithPadding a b c xs ys [c]

nspaces :: Int -> String
nspaces i = take i (repeat ' ')
