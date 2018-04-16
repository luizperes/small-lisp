module Unparser where

import Parser
import Data.Char (isAlpha)

unparse :: SExpression -> [Char]
unparse (List lst) = "(" ++ (foldl (\acc l -> acc ++ " " ++ (unparse l)) "" lst) ++ " )"
unparse (SymAtom s) = s
unparse (NumAtom n) = show n
