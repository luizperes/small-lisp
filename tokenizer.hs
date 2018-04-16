module Tokenizer where

import System.Environment 
import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf)

{- 
 - comment: (^;;;.*$)+
 - numeral: -?[0-9]+
 - alphanumeric_symbol: [A-Za-z](-?[A-Za-z0-9])*
 - special_symbol: [+-*/<>=&|!@#$%?:]+
 - lparen: \(
 - rparen: \)
 - lbrak: \[
 - rbrak: \]
 - lbrace: \{
 - rbrace: \}
 - equal: =
 - semicolon: ;
 - arrow: -->
 - quote-mark: "
 - colon: :
 -}

data Token = Comment [Char]
           | NumToken Int
           | AlphaNumToken [Char]
           | SpecialToken [Char]
           | Lparen | Rparen | Lbrak | Rbrak | LBrace | RBrace
           | Equal | Semicolon | Arrow | Quote | Colon | Exclamation | At
           deriving (Eq, Show)

tokenizeMany :: [[Char]] -> [Char] -> Maybe [Token]
tokenizeMany [] [] = Nothing
tokenizeMany [] acc = tokenize acc
tokenizeMany (x:xs) acc = tokenizeMany xs (acc ++ " " ++ x)

tokenize :: [Char] -> Maybe [Token]
tokenize str =
  case getTokens ([], str) of
    Just (tks, []) -> Just tks
    _ -> Nothing

getTokens :: ([Token], [Char]) -> Maybe ([Token], [Char])
getTokens (tks, []) = Just (tks, [])
getTokens (tks, str) =
  case findNextToken (tks, str) of
    Just (newTks, newStr) -> getTokens (newTks, newStr)
    _ -> Nothing

findNextToken :: ([Token], [Char]) -> Maybe ([Token], [Char])
findNextToken (tks, []) = Just (tks, [])
findNextToken (tks, '\n':str) = findNextToken (tks, str)
findNextToken (tks, '\t':str) = findNextToken (tks, str)
findNextToken (tks, ' ':str) = findNextToken (tks, str)
findNextToken (tks, '(':str) = Just (tks ++ [Lparen], str)
findNextToken (tks, ')':str) = Just (tks ++ [Rparen], str)
findNextToken (tks, '[':str) = Just (tks ++ [Lbrak], str)
findNextToken (tks, ']':str) = Just (tks ++ [Rbrak], str)
findNextToken (tks, '{':str) = Just (tks ++ [LBrace], str)
findNextToken (tks, '}':str) = Just (tks ++ [RBrace], str)
findNextToken (tks, ';':str) =
  case str of
    ';':';':newStr ->
      case getAnythingTill str '\n' "" of
        (commentStr, rest) -> Just (tks ++ [Comment commentStr], rest)
    _ -> Just (tks ++ [Semicolon], str)
findNextToken (tks, '\"':str) = Just (tks ++ [Quote], str)
findNextToken (tks, c:str)
  | isAlpha c = getIdentifier (tks, c:str) ""
  | isDigit c = getNumber (tks, c:str) ""
  | c == '-' && isDigit(head str) = getNumber (tks, str) "-"
  | c == ':' && not(isSpecialSymbol(head str)) = Just (tks ++ [Colon], str)
  | c == '=' && not(isSpecialSymbol(head str)) = Just (tks ++ [Equal], str)
  | c == '!' && not(isSpecialSymbol(head str)) = Just (tks ++ [Exclamation], str)
  | c == '@' && not(isSpecialSymbol(head str)) = Just (tks ++ [At], str)
  | "-->" `isPrefixOf` (c:str) && not(isSpecialSymbol(head(tail(tail str)))) = Just (tks ++ [Arrow], tail(tail str))
  | isSpecialSymbol c = getSpecialSymbol (tks, c:str) ""
  | otherwise = Nothing

isSpecialSymbol :: Char -> Bool
isSpecialSymbol c = c `elem` "+-*/<>=&|!@#$%?:"

getIdentifier :: ([Token], [Char]) -> [Char] -> Maybe ([Token], [Char])
getIdentifier (tks, []) res = Just (tks ++ [AlphaNumToken res], [])
getIdentifier (tks, c:str) res
  | isAlpha c || isDigit c = getIdentifier (tks, str) (res ++ [c])
  | c == '-' && (isAlpha (head str) || (isDigit (head str))) = getIdentifier (tks, str) (res ++ ['-'])
  | otherwise = Just (tks ++ [AlphaNumToken res], c:str)

getNumber :: ([Token], [Char]) -> [Char] -> Maybe ([Token], [Char])
getNumber (tks, []) res = Just (tks ++ [NumToken (read res::Int)], [])
getNumber (tks, c:str) res
  | isDigit c = getNumber (tks, str) (res ++ [c])
  | otherwise = Just (tks ++ [NumToken (read res::Int)], c:str)

getSpecialSymbol :: ([Token], [Char]) -> [Char] -> Maybe ([Token], [Char])
getSpecialSymbol (tks, []) res = Just (tks ++ [SpecialToken res], [])
getSpecialSymbol (tks, c:str) res
  | isSpecialSymbol c = getSpecialSymbol (tks, str) (res ++ [c])
  | otherwise = Just (tks ++ [SpecialToken res], c:str)

getAnythingTill :: [Char] -> Char -> [Char] -> ([Char], [Char])
getAnythingTill "" c res = (res, "")
getAnythingTill (w:str) c res
  | w == c = (res ++ [w], str)
  | otherwise = getAnythingTill str c (res ++ [w])
