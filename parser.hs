module Parser where

import Tokenizer
import Control.Applicative

{-
 - <S-expression> ::= <atom> | <list>
 - <atom> ::= <numeric-atom> | <symbolic-atom>
 - <list> ::= '(' {<S-expression>} ')
 - <numeric-atom> = -?[0-9]+
 - <symbolic-atom> = [A-Za-z](-?[A-Za-z0-9])*|[+-*/<>=&|!@#$%?:]+
 -
 - <identifier> = [A-Za-z](-?[A-Za-z0-9])*
 - <expression> ::= <value> | <variable> | <function-call> |
 -                  <conditional-expression> | <let-expression>
 - <value> ::= <numeric-atom> | '"' <symbolic-atom> '"' | <list>
 -
 - <variable> ::= <identifier>
 -
 - <function-call> ::= <function-identifier> '[' <expression> {';' <expression>} ']'
 - <function-identifier> ::= <identifier>
 -
 - <conditional-expression> ::= '[' <clause> {';' <clause>}]
 - <clause> ::= <expression> '-->' <expression>
 -
 - <let-expression> ::= '{' <local-definition> {';' <local-definition>} ':' <expression> '}'
 - <local-definition> ::= <identifier> '=' <expression>
 - <program> ::= {<definition>}
 -
 - <definition> ::= <constant-definition> | <function-definition>
 - <constant-definition> ::=
 -  [<comment>] <identifier> '=' <expression>
 -
 - <function-definition> ::=
 -  [<comment>]
 -  <function-identifier> '[' <parameter> {';' <parameter> }']' '=' <expression>
 - <parameter> ::= <identifier>
 -
 - <comment> = (^;;;.*$)*
 -
 - <map-expression> ::= @ <function-name> [ <arguments-list> ]
 - <arguments-list> ::= <expression> {; <expression>}
 -
 - <reduce-expression> ::= ! <function-name> [ <expression> ]
 -}

data SExpression = NumAtom Int
                 | SymAtom [Char]
                 | List [SExpression]
                 deriving (Show, Ord, Eq)

type Identifier = [Char]

data SmLispExpr = SExpr SExpression
                | Variable Identifier
                | FnCall Identifier [SmLispExpr]
                | CondExpr [CondClause]
                | LetExpr [LocalDef] SmLispExpr
                | MapExpr Identifier [SmLispExpr]
                | ReduceExpr Identifier SmLispExpr
                deriving (Show, Ord, Eq)

type CondClause = (SmLispExpr, SmLispExpr)

type LocalDef = (Identifier, SmLispExpr)

type SmComment = [Char]

data Definition = ConstantDef SmComment Identifier SmLispExpr
                | FunctionDef SmComment Identifier [Identifier] SmLispExpr
                deriving (Show, Ord, Eq)

type SmLispProgram = [Definition]

parseSExpression :: [Token] -> (Maybe SExpression, [Token])
parseSExpression [] = (Nothing, [])
parseSExpression (Lparen:more) =
  case parseSList more [] of
    (Just se, yet_more) -> (Just se, yet_more)
    _ -> (Nothing, Lparen:more)
parseSExpression tks = 
  case parseAtom tks of
    (Just s, newTks) -> (Just s, newTks)
    _ -> (Nothing, tks)

parseSList :: [Token] -> [SExpression] -> (Maybe SExpression, [Token])
parseSList (Rparen:tks) acc = (Just (List acc), tks)
parseSList tks acc =
  case parseSExpression tks of
    (Just se, newTks) -> parseSList newTks (acc ++ [se])
    _ -> (Nothing, tks)

parseAtom :: [Token] -> (Maybe SExpression, [Token])
parseAtom tks =
  case tks of
    ((NumToken x):newTks) -> (Just (NumAtom x), newTks)
    ((AlphaNumToken v):newTks) -> (Just (SymAtom v), newTks)
    ((SpecialToken v):newTks) -> (Just (SymAtom v), newTks)
    _ -> (Nothing, tks)

parseSmLispExpr :: [Token] -> (Maybe SmLispExpr, [Token])
parseSmLispExpr [] = (Nothing, [])
parseSmLispExpr (Quote:tks) =
  case parseSExpression tks of
    (Just (SymAtom atom), Quote:newTks) -> (Just(SExpr(SymAtom atom)), newTks)
    _ -> (Nothing, Quote:tks)
parseSmLispExpr ((AlphaNumToken v):tks) =
  case tks of
    (Lbrak:more) -> case parseSmExprList v more [] of
      (Just se, yet_more) -> (Just se, yet_more)
      _ -> (Nothing, (AlphaNumToken v):Lbrak:more)
    _ -> (Just (Variable v), tks)
parseSmLispExpr (At:tks) =
  case tks of
    ((AlphaNumToken v):more) ->
      case more of
        (Lbrak:yet_more) ->
          case parseSmExprList v yet_more [] of
            (Just (FnCall id se), newTks) -> (Just (MapExpr id se), newTks)
            _ -> (Nothing, At:tks)
        _ -> (Just (Variable v), tks)
    _ -> (Nothing, At:tks)
parseSmLispExpr (Exclamation:tks) =
  case tks of
    ((AlphaNumToken v):more) ->
      case more of
        (Lbrak:yet_more) ->
          case parseSmExprList v yet_more [] of
            (Just (FnCall id (se:[])), newTks) -> (Just (ReduceExpr id se), newTks)
            _ -> (Nothing, Exclamation:tks)
        _ -> (Just (Variable v), tks)
    _ -> (Nothing, Exclamation:tks)
parseSmLispExpr ((Lbrak):tks) =
  case parseConditionalExpr tks [] of
    (Just (CondExpr expr), newTks) -> (Just(CondExpr expr), newTks)
    _ -> (Nothing, tks)
parseSmLispExpr ((LBrace):tks) =
  case parseLetExpr tks [] of
    (Just (LetExpr defs expr), newTks) -> (Just(LetExpr defs expr), newTks)
    _ -> (Nothing, tks)
parseSmLispExpr tks =
  case parseSExpression tks of
    (Just s, newTks) -> (Just (SExpr s), newTks)
    _ -> (Nothing, tks)

parseSmExprList :: [Char] -> [Token] -> [SmLispExpr] -> (Maybe SmLispExpr, [Token])
parseSmExprList id [] acc = (Nothing, [])
parseSmExprList id tks acc =
  case parseSmLispExpr tks of
    (Just se, Semicolon:newTks) -> parseSmExprList id newTks (acc ++ [se])
    (Just se, Rbrak:newTks) -> (Just (FnCall id (acc ++ [se])), newTks)
    _ -> (Nothing, tks)

parseConditionalExpr :: [Token] -> [CondClause] -> (Maybe SmLispExpr, [Token])
parseConditionalExpr [] acc = (Nothing, [])
parseConditionalExpr tks acc =
  case parseSmClause tks of
    (Just se, Semicolon:newTks) -> parseConditionalExpr newTks (acc ++ [se])
    (Just se, Rbrak:newTks) -> (Just (CondExpr (acc ++ [se])), newTks)
    _ -> (Nothing, tks)

parseSmClause :: [Token] -> (Maybe CondClause, [Token])
parseSmClause tks =
  case parseSmLispExpr tks of
    (Just se, Arrow:more) ->
      case parseSmLispExpr more of
        (Just se2, yet_more) -> (Just(se, se2), yet_more)
        _ -> (Nothing, tks)
    _ -> (Nothing, tks)

parseLetExpr :: [Token] -> [LocalDef] -> (Maybe SmLispExpr, [Token])
parseLetExpr [] acc = (Nothing, [])
parseLetExpr tks acc =
  case parseLocalDef tks of
    (Just ld, Semicolon:newTks) -> parseLetExpr newTks (acc ++ [ld])
    (Just ld, Colon:newTks) ->
      case parseSmLispExpr newTks of
        (Just se, RBrace:more) -> (Just (LetExpr (acc ++ [ld]) se), more)
        _ -> (Nothing, tks)
    _ -> (Nothing, tks)

parseLocalDef :: [Token] -> (Maybe LocalDef, [Token])
parseLocalDef [] = (Nothing, [])
parseLocalDef (tk:[]) = (Nothing, tk:[])
parseLocalDef tks =
  case tks of
    ((AlphaNumToken v):Equal:more) ->
      case parseSmLispExpr more of
        (Just se, yet_more) -> (Just (v, se), yet_more)
        _ -> (Nothing, tks)
    _ -> (Nothing, tks)

parseSmLispProgram :: [Token] -> (Maybe SmLispProgram, [Token])
parseSmLispProgram [] = (Nothing, [])
parseSmLispProgram tks =
  case parseDefinitions tks [] of
    (defs, []) -> (Just defs, [])
    (_, tks) -> (Nothing, tks)

parseDefinitions :: [Token] -> [Definition] -> ([Definition], [Token])
parseDefinitions tks defs =
  case parseComments tks "" of
    (cmts, more) -> case more of
      [] -> (defs, [])
      (a:[]) -> (defs, more)
      (a:b:[]) -> (defs, more)
      ((AlphaNumToken v):Equal:yet_more) -> case parseSmLispExpr yet_more of
        (Just def, []) -> (defs ++ [(ConstantDef cmts v def)], [])
        (Just def, newTks) -> parseDefinitions newTks (defs ++ [(ConstantDef cmts v def)])
        (_, newTks) -> (defs, newTks)
      ((AlphaNumToken v):Lbrak:yet_more) -> case parseFunDef yet_more [] of
        (Just (params, def), []) -> (defs ++ [(FunctionDef cmts v params def)], [])
        (Just (params, def), newTks) -> parseDefinitions newTks (defs ++ [(FunctionDef cmts v params def)])
        (_, newTks) -> (defs, newTks)
      _ -> (defs, more)

parseFunDef :: [Token] -> [Identifier] -> (Maybe ([Identifier], SmLispExpr), [Token])
parseFunDef [] acc = (Nothing, [])
parseFunDef tks acc =
  case tks of
    ((AlphaNumToken v):Semicolon:newTks) -> parseFunDef newTks (acc ++ [v])
    ((AlphaNumToken v):Rbrak:Equal:newTks) ->
      case parseSmLispExpr newTks of
        (Just se, more) -> (Just ((acc ++ [v]), se), more)
        _ -> (Nothing, tks)
    _ -> (Nothing, tks)

parseComments :: [Token] -> [Char] -> (SmComment, [Token])
parseComments [] cmts = (cmts, [])
parseComments ((Comment cmt):tks) cmts = parseComments tks (cmts ++ cmt)
parseComments (tk:tks) cmts = (cmts, (tk:tks))
