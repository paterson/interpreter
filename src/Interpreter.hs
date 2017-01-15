module Interpreter
   (Program, run, verify, programFromText, testProgram, failingProgram
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
exec (Seq s0 s1)          = do exec (Debug s0) >> exec (Debug s1)
exec (Print e)            = do st <- get
                               Right val <- return $ runEval (currentEnv st) (eval e)
                               put $ ProgramState{ envs = (envs st), statements = (statements st), outputs = val:(outputs st) }
                               return ()
exec (If cond s0 s1)      = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec (Debug s0) else do exec (Debug s1)
exec (While cond s)       = do st <- get
                               Right (B val) <- return $ runEval (currentEnv st) (eval cond)
                               if val then do exec (Debug s) >> exec DebugTidyTree >> exec (Debug (While cond s)) else return()
exec (Try s0 s1)          = do catchError (exec s0) (\_ -> exec s1)
exec Pass                 = return ()
exec (Debug (Seq s0 s1))  = exec (Seq s0 s1)                     -- Ignore this case as it's just chaining
exec (Debug s)            = do tick s
                               printTree s
                               printInstructions
                               prompt s
exec DebugTidyTree        = do st <- get
                               clearFromTree (lastStatement st)

-- Executing a "program" means compiling it and then running the
-- resulting Statement with an empty variable map.
run :: Program -> IO ()
run p = do result <- runExceptT $ (runStateT (exec (compile p)) initialProgramState)
           case result of
             Right ((), _) -> return ()
             Left exn      -> System.print $ "Uncaught exception: " ++ exn

-- ** Prompts **
-- Pattern match on input and first character so we can ensure the inspect command starts with a colon.
-- Re-prompt if string is empty (this also prevents head input from failing because of lazy evaluation)
prompt :: Statement -> Run ()
prompt s = do input <- liftIO $ getLine
              case (input, head input) of
                ("",_)     -> prompt s
                ("next",_) -> exec s
                ("back",_) -> do stepback
                                 exec (Debug s)
                (_,':')    -> do inspect $ tail input
                                 printHistoryInstructions
                                 historyprompt s
                (_,'|')    -> exec (read (tail input) :: Statement)
                (_,_)      -> do liftIO $ putStrLn . red $ "Unknown command " ++ input
                                 printInstructions
                                 prompt s

historyprompt :: Statement -> Run ()
historyprompt s = do input <- liftIO $ getLine
                     case input of
                       "done" -> do printTree s
                                    printInstructions
                                    prompt s
                       _      -> return ()

-- ** Operations **

tick :: Statement -> Run ()
tick s = do st <- get
            put $ ProgramState{ envs = (currentEnv st):(envs st), statements = s:(statements st), outputs = (outputs st) }

stepback :: Run ()
stepback = do st <- get
              let statement = lastStatement st
              put $ ProgramState{ envs = (tail $ (tail $ envs st)), statements = (tail $ (tail $ statements st)), outputs = (outputs st) }
              exec (Debug statement)

inspect :: String -> Run ()
inspect v = printVarHistory v

clearFromTree :: Statement -> Run ()
clearFromTree s = do st <- get
                     put $ ProgramState{ envs = (envs st), statements = (tail $ statements st), outputs = (outputs st) }
                     case s of
                       (While _ _) -> return ()
                       _           -> clearFromTree $ lastStatement st

-- ** Print operations **

printInstructions :: Run ()
printInstructions = liftIO $ putStrLn "Enter 'next' to proceed, 'back' to step back and ':<variablename>' to view a variable's value"

printHistoryInstructions :: Run ()
printHistoryInstructions = liftIO $ putStrLn "Enter 'done' to return."

-- Takes in next statement and prints it with the rest of the history tree
printTree :: Statement -> Run ()
printTree s = do st <- get
                 let statements' = tail $ statements st
                 let list = zipWithPadding Pass ("", Null) Null (reverse statements') (Map.toList (currentEnv st)) (reverse (outputs st))
                 clearTerminal
                 liftIO $ printTreeHeader
                 liftIO $ mapM_ (putStrLn . printTreeLine) $ list
                 liftIO $ putStrLn $ printCurrentStatement s

printVarHistory :: Name -> Run()
printVarHistory v = do st <- get
                       let statements' = tail $ statements st
                       let envs'       = tail $ envs st
                       let varhist     = reverse $ map (variableFromEnv v) envs'
                       let list = zipWithPadding Pass ("", Null) Null (reverse statements') varhist []
                       clearTerminal
                       liftIO $ printTreeHeader'
                       liftIO $ mapM_ (putStrLn . printTreeLine) $ list

-- Tree header for normal tree
printTreeHeader :: IO ()
printTreeHeader = do liftIO $ putStrLn $ "Statements" ++ (nspaces (60-10)) ++ "Variables" ++ (nspaces (60-9)) ++ "Output"
                     liftIO $ putStrLn $ "==========" ++ (nspaces (60-10)) ++ "=========" ++ (nspaces (60-9)) ++ "======"

-- Tree header for history tree
printTreeHeader' :: IO ()
printTreeHeader' = do liftIO $ putStrLn $ "Statements" ++ (nspaces (60-10)) ++ "Value at Point of execution"
                      liftIO $ putStrLn $ "==========" ++ (nspaces (60-10)) ++ "==========================="

printTreeLine :: (Statement, (Name, Val), Val) -> String
printTreeLine (s, var, val) = (cyan $ statementToString s) ++ spaces ++ (printVariable var) ++ spaces' ++ (printOutput val)
                              where spaces  = nspaces (60 - l)
                                    spaces' = nspaces (60 - l')
                                    l       = length $ statementToString s
                                    l'      = length $ printVariable var

printCurrentStatement :: Statement -> String
printCurrentStatement s = green $ "> " ++ statementToString s

printVariable :: (Name, Val) -> String
printVariable (_, Null) = ""
printVariable (n, v)    = "Variable: " ++ (blue n) ++ " Value: " ++ (blue (show v))

printOutput :: Val -> String
printOutput Null = ""
printOutput s    = show s

-- ** Utils **

variableFromEnv :: Name -> Env -> (Name, Val)
variableFromEnv var env = case Map.lookup var env of
                              Just val -> (var, val)
                              Nothing  -> ("", Null)

statementToString :: Statement -> String
statementToString (While cond _) = "While " ++ (show cond) ++ " do"
statementToString (If cond _ _)  = "If " ++ (show cond) ++ " do"
statementToString Pass           = ""                                  -- Don't bother printing pass statements
statementToString s              = show s

-- Clear screen twice ensures we are at the bottom of the screen.
clearTerminal :: Run ()
clearTerminal = do liftIO $ clearScreen
                   liftIO $ clearScreen

-- Normalize's lengths of zipped lists (So will fill with standard defaults)
zipWithPadding :: a -> b -> c -> [a] -> [b] -> [c] -> [(a,b,c)]
zipWithPadding a b c (x:xs) (y:ys) (z:zs) = (x,y,z) : zipWithPadding a b c xs ys zs
zipWithPadding _ _ _ []     []     []     = []
zipWithPadding a b c []     ys     zs     = zipWithPadding a b c [a] ys zs
zipWithPadding a b c xs     []     zs     = zipWithPadding a b c xs [b] zs
zipWithPadding a b c xs     ys     []     = zipWithPadding a b c xs ys [c]

nspaces :: Int -> String
nspaces n = take n (repeat ' ')
