module Main where

import Parser
import Codegen
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule =
  emptyModule "The Knæ Language"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname =
  readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine ">>> "
      case minput of
        Nothing -> outputStrLn "yes bye"
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Just modn -> loop modn
            Nothing   -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
