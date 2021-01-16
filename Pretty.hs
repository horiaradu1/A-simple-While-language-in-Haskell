{-
  Implement a pretty-printer for expressions and commands.
  Also refer to the syntax description in Readme.md!

  Here are the rules. As they are in English text, they might be ambiguous, so
  refer to the given test-sets for disambiguation!
  Aspects not specified by the rules can be implemented as you like (As long as you can parse your own output in the second part of this exercise).

  Expressions:
    As a general rule, pretty-printing shall not drop any information in the AST.
    In particular, the association of binary operators shall still be visible, i.e.
    do print Plus a (Plus b c) as "a+(b+c)" (and NOT as "a+b+c")

    * Only print parenthesis if they are required. In particular:

      Don't print parenthesis if given by operator priority, e.g.
        a*b+c instead of (a*b)+c

      Don't print parenthesis if given by operator associativity. All operators associate to the left!
      E.g., print:
        a-b-c instead of (a-b)-c
        a+b+c instead of (a+b)+c

      Unary minus: regard the unary minus rules in Readme.md, and be careful to not confuse the ASTs
        for "Uminus (Const x)" and "Const (-x)", e.g.

        Uminus (Const 42) -> "-(42)"
        Const (-x) -> -42


  Commands:
  * Separate commands in sequential composition by ;, use { and } as delimiters

    * insert line breaks
        after: do, then, else, command in sequential composition
      * Exception: do not insert line-breaks between {,} and then,else,do
      * Exception: do not insert line break between else and if

    * increase level of indentation inside if and while. Use 2 spaces per level of indentation.

    * Delimit all Seq-blocks with {}, even if nested

    E.g.

    {}   (for Seq [])

    {
      {}
    }

    {
      x = 1
    }

    {
      x = 1;
      y = 2
    }

    while c>0 do {
      x=x*2;
      c=c-1
    }

    if b then
      x=1
    else {
      x=x*3;
      y=1
    }

    if b then {
      x=1
    } else if c then {
      x=2;
      b=1
    } else {
      x=3
    }



-}
module Pretty (prettyExp, prettyCom) where
import While

{- Add your import declarations here! -}


prettyExp :: Exp -> String

prettyExp e = pretty e "empty" 0


-- For just a Variable
-- pretty :: (Num t, Eq t) => Exp -> [Char] -> t -> [Char]
pretty (Var name) _ _ = name
-- For just a Constant
pretty (Const integer) before _
                        | before == "uminus" = "(" ++ show integer ++ ")"
                        | otherwise = show integer
-- For Unary Minus
pretty (Uminus expr) before _
                          | before == "uminus" = "(-" ++ pretty expr "uminus" 0 ++ ")" 
                          | otherwise = "-" ++ pretty expr "uminus" 0

-- For Times and Division
pretty (Binop Times x y) before loc
                                  | before == "uminus" || (before == "timesdiv" && loc == 1) = "(" ++ pretty x "timesdiv" 0 ++ "*" ++ pretty y "timesdiv" 1 ++ ")"
                                  | otherwise = pretty x "timesdiv" 0 ++ "*" ++ pretty y "timesdiv" 1

pretty (Binop Div x y) before loc
                                | before == "uminus" || (before == "timesdiv" && loc == 1) = "(" ++ pretty x "timesdiv" 0 ++ "/" ++ pretty y "timesdiv" 1 ++ ")"
                                | otherwise = pretty x "timesdiv" 0 ++ "/" ++ pretty y "timesdiv" 1

-- For Plus and Minus
pretty  (Binop Plus x y) before loc
                                  | before == "uminus" || before == "timesdiv" || (before == "plusminus" && loc == 1) = "(" ++ pretty x "plusminus" 0 ++ "+" ++ pretty y "plusminus" 1 ++ ")"
                                  | otherwise = pretty x  "plusminus" 0++ "+" ++ pretty y "plusminus" 1

pretty (Binop Minus x y) before loc
                                  | before == "uminus" || before == "timesdiv" || (before == "plusminus" && loc == 1) = "(" ++ pretty x "plusminus" 0 ++ "-" ++ pretty y "plusminus" 1 ++ ")"
                                  | otherwise = pretty x "plusminus" 0 ++ "-" ++ pretty y "plusminus" 1 
                                    
-- For Comparisons (Less Than, Less Than and Equal, Equal)
pretty (Binop Less x y) before loc
                                | before == "empty" || before == "and" || before == "or" || (before == "comparison" && loc == 0) = pretty x "comparison" 0 ++ "<" ++ pretty y "comparison" 1 
                                | otherwise = "(" ++ pretty x "comparison" 0 ++ "<" ++ pretty y "comparison" 1 ++ ")"

pretty (Binop LessEq x y) before loc
                                  | before == "empty" || before == "and" || before == "or" || (before == "comparison" && loc == 0) = pretty x "comparison" 0 ++ "<=" ++ pretty y "comparison" 1
                                  | otherwise = "(" ++ pretty x "comparison" 0 ++ "<=" ++ pretty y "comparison" 1 ++ ")"

pretty (Binop Equal x y) before loc
                                  | before == "empty" || before == "and" || before == "or" || (before == "comparison" && loc == 0) = pretty x "comparison" 0 ++ "==" ++ pretty y "comparison" 1
                                  | otherwise = "(" ++ pretty x "comparison" 0 ++ "==" ++ pretty y "comparison" 1 ++ ")"

-- For And & Or
pretty (Binop And x y) before loc
                                | before == "empty" || (before == "and" && loc == 0) || before == "or" = pretty x "and" 0 ++ "&&" ++ pretty y "and" 1
                                | otherwise = "(" ++ pretty x "and" 0 ++ "&&" ++ pretty y "and" 1 ++ ")"

pretty (Binop Or x y) before loc
                              | before == "empty" || (before == "or" && loc == 0) = pretty x "or" 0 ++ "||" ++ pretty y "or" 1
                              | otherwise = "(" ++ pretty x "or" 0 ++ "||" ++ pretty y "or" 1 ++ ")"



prettyCom :: Com -> String

prettyCom c = prettyC c "empty"

prettyC (Seq comp) before
                  | null comp = "{}"
                  | otherwise = "{\n" ++ enlist comp ++ "\n}" where
                                         enlist comp | length comp == 1 = prettyC (head comp) "seq"
                                                     | otherwise = " " ++ prettyC (head comp) "seq" ++ ";\n " ++ enlist (tail comp)

prettyC (Assign name expr) before = name ++ "=" ++ prettyExp expr

prettyC (If cond expr1 expr2) before = "if " ++ prettyExp cond ++ " then\n " ++ prettyC expr1 "if" ++ " \nelse " ++ prettyC expr2 "if"

prettyC (While cond expr) before = "while " ++ prettyExp cond ++ " do\n " ++ prettyC expr "while"
