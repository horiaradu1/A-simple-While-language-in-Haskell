{-
  Provide functions
    parseExp :: String -> Maybe Exp
    parseCom :: String -> Maybe Com


  to parse expressions and commands.
  You can use whatever parser technology you wish, however, we recommend using
  the simple parser combinators from the lecture (which is distributed in PComb.hs with this assignment),
  or the more advanced parsec framework.

  See Readme.md for the syntax of the language.
-}

module Parse (parseExp,parseCom) where
import While

{- Add your import declarations here -}

import PComb
-- ctrl
import Control.Monad
import Control.Applicative(empty, (<|>), Alternative)


parseExp :: String -> Maybe Exp
parseExp =  parseAll s

s = orExp

-- For variables and constants, paranthesis, unary minuses or the following down below
expSelector =
      Var <$> ident
    <|> 
      Const <$> number
    <|> 
      paranthesis
    <|> 
      uminus

paranthesis = do 
                 ctrl "(";
                 exp <- s;
                 ctrl ")";
                 return exp

-- For Unary Minus
uminus =
    do 
       ctrl "-(";
       exp <- s;
       ctrl ")";
       return (Uminus exp)
   <|>
    do 
       ctrl "-";
       Uminus . Var <$> ident

-- Function to be used instead of original "foldl", considering the Binary Operation
foldlAssoc :: Exp -> [(Binop, Exp)] -> Exp
foldlAssoc exp exp_long | length exp_long == 1 = Binop (fst (head exp_long)) exp (snd (head exp_long))
                        | null exp_long = exp
                        | otherwise = foldlAssoc (Binop (fst (head exp_long)) exp (snd (head exp_long))) (tail exp_long)

-- NEW RULE
-- For "*" and "/"
timesDiv =
   do 
      exp <- expSelector
      (do exp_long <- some after
          return (foldlAssoc exp exp_long))
      <|> 
      (do exp <- expSelector
          return exp)
   where
      after =
            (do ctrl "*"; check <- expSelector; return (Times, check))
            <|>
            (do ctrl "/"; check <- expSelector; return (Div, check))

-- NEW RULE
-- For "+" and "-"
plusMinus =
   do 
      exp <- timesDiv
      (do exp_long <- some after
          return (foldlAssoc exp exp_long))
      <|> 
      (do exp <- timesDiv
          return exp)
   where
      after = 
            (do ctrl "+"; check <- timesDiv; return (Plus, check))
            <|>
            (do ctrl "-"; check <- timesDiv; return (Minus, check))

-- NEW RULE
-- For "<=", "<", "=="
comparisons =
   do 
      exp <- plusMinus
      (do exp_long <- many after
          return (foldlAssoc exp exp_long))
      <|> 
      (do exp <- plusMinus
          return exp)
   where
      after =
            (do ctrl "<=";
                check <- plusMinus;
                return (LessEq, check))
            <|>
            (do ctrl "<"; 
                check <- plusMinus;
                return (Less, check))
            <|>
            (do ctrl "==";
                check <- plusMinus;
                return (Equal, check))

-- NEW RULE
-- For "&&"
andExp =
   do 
      exp <- comparisons
      (do exp_long <- many after
          return (foldlAssoc exp exp_long))
      <|> 
      (do exp <- comparisons
          return exp)
  where
     after = do ctrl "&&"; check <- comparisons; return (And, check)

-- For "||"
orExp =
   do 
      exp <- andExp
      (do exp_long <- many after
          return (foldlAssoc exp exp_long))
      <|> 
      (do exp <- andExp
          return exp)
  where
     after = do ctrl "||"; check <- andExp; return (Or, check)


parseCom :: String -> Maybe Com
parseCom = parseAll command
command = comAssign <|> comSeq <|> comIf <|> comWhile -- Choosing the corect command

comAssign =
   do
      com <- ident;
      ctrl "=";
      Assign com <$> s

comSeq =
   do
      ctrl "{";
      comp <- PComb.sepBy command (ctrl ";");
      ctrl "}";
      do return $ Seq comp

comIf =
   do
      PComb.keyword "if";
      condition <- s;
      PComb.keyword "then";
      expression <- command;
      PComb.keyword "else";
      If condition expression <$> command

comWhile =
   do
      PComb.keyword "while";
      condition <- s;
      PComb.keyword "do";
      While condition <$> command



