import System.Environment
import System.IO
import Control.Monad

import Tokenizer
import Parser
import Interpreter

read' :: IO [Char]
read' = putStr "REPL> "
     >> hFlush stdout
     >> getLine

print' :: String -> IO ()
print' = putStrLn

eval' :: [Definition] -> [Char] -> [Char]
eval' defs input =
  case tokenize input of
    Just tks ->
      case parseSmLispExpr tks of
        (Just se, []) -> interpret defs se
        _ -> "Could not parse expr:" ++ (show tks)
    _ -> "Could not tokenize expr:" ++ input

main = do
  args <- getArgs
  srcText <- mapM readFile args
  case tokenizeMany srcText "" of
    Just x ->
      case parseSmLispProgram x of
        (Just prog, []) -> do
          input <- read'
          unless (input == ":q")
            $ print' (eval' prog input) >> main
        (Nothing, []) -> print ""
        (s, t) -> print ("Could not parse tokens:" ++ (show t))
    _ -> print "Could not tokenize input. Usage: ./assig3 <path-to-file-1> {... <path-to-file-n>} "
