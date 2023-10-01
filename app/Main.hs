module Main (main) where

import Control.Monad
import Cuderia.Syntax.Parser
import Cuderia.VM.Interpreter
import Data.IORef (readIORef)
import Data.Text qualified as T
import GHC.IORef
import System.IO

main :: IO ()
main = do
  ipref <- newIORef newInterpreter
  forever $ do
    putStr "ξﾟ⊿ ﾟ)ξ > "
    hFlush stdout
    line <- hGetLine stdin
    ip <- readIORef ipref
    case parse "(stdin)" (T.pack line) of
      Left err -> putStrLn $ "Parse error: " ++ show err
      Right program -> do
        let (newip, result) = runInterpreter ip program
        case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right val -> do
            writeIORef ipref newip
            putStrLn . T.unpack $ display val