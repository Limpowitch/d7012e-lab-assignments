module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Sequencial [Statement]
    | Skip
    | Write Expr.T
    | Read String
    | Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

repeat_ :: Parser Statement
repeat_ =
    accept "repeat"
    -# parse            -- Statement body
    #- require "until"
    # Expr.parse        -- Repeat condition
    #- require ";"
    >-> buildRepeat_

buildRepeat_ :: (Statement, Expr.T) -> Statement
buildRepeat_ (stmt, cond) = Repeat stmt cond

if_ :: Parser Statement -- example: _if "if x then skip; else read y;" will return: Just (((Var "x", Skip), Read "y"), "")
if_ =
    accept "if"
    -# Expr.parse -- Parse if-condition
    #- require "then"
    # parse       -- Parse then-branch
    #- accept "else"
    # parse       -- Parse else-branch
    >-> buildIf_

buildIf_ :: ((Expr.T, Statement), Statement) -> Statement
buildIf_ ((c, t), e) = If c t e

while_ :: Parser Statement -- example: while_ "while n do n:= n-1;" == Just ((Var "n", Assignment) )
while_ =
    accept "while"
    -# Expr.parse -- Parse while-condition
    #- require "do"
    # parse       -- Parse the loop-body
    >-> buildWhile_

buildWhile_ :: (Expr.T, Statement) -> Statement
buildWhile_ (c, t) = While c t

secuencial_ :: Parser Statement
secuencial_ =
    accept "begin"
    -# iter parse -- iteratively parse the expressions between begin and end
    #- require "end"
    >-> buildSecuencial_

buildSecuencial_ :: [Statement] -> Statement
buildSecuencial_ = Sequencial

skip_ :: Parser Statement
skip_ =
    accept "skip"
    # require ";" -- Only thing we care about, has the type 'a'
    >-> buildSkip_

buildSkip_ :: a -> Statement
buildSkip_ _ = Skip

write_ :: Parser Statement
write_ =
    accept "write"
    -# Expr.parse
    #- require ";"
    >-> buildWrite_

buildWrite_ :: Expr.T -> Statement
buildWrite_ = Write

read_ :: Parser Statement
read_ =
    accept "read"
    -# word
    #- require ";"
    >-> buildRead_

buildRead_ :: String -> Statement
buildRead_ = Read

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Assignment var expr : stmts) dict input =
  let val   = Expr.value expr dict                  -- We evaluate the expression, and lookup any defined variables in our dictionary dict
      dict' = Dictionary.insert (var, val) dict     -- We update our dictionary filled with assigned variables, with our newly assigned variable-value pair 
  in exec stmts dict' input                         -- Continue executing the rest of the statements if we have any

exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond body : stmts) dict input =
    if Expr.value cond dict>0                             -- We do a similar check as with our 'If' exec function. Just check if expression is true
    then exec (body : While cond body : stmts) dict input -- If True: run the loop body, then re-enqueue the same While so it repeats
    else exec stmts dict input                            -- If False: drop the loop body and execute following statements (break the loop)

exec (Sequencial statement : stmts) dict input =
    exec (statement ++ stmts) dict input                 -- We simply concat the list of statements and execute them sequencially 

exec (Skip : stmts ) dict input = exec stmts dict input   -- We do nothing and more on to the following statements (we skip, simple as that)

exec (Write expr : stmts) dict input =
    Expr.value expr dict : exec stmts dict input    -- We simply check the value of the given expression, and do that for all following expressions

exec (Read var : stmts) dict (i:is) = 
    let dict' = Dictionary.insert (var, i) dict     -- We insert the value 'i' for variable 'var' in the dictionary
    in exec stmts  dict' is                         -- We continue this for all following variables/statements

exec (Repeat stmt cond: stmts) dict input =
    exec (stmt: If cond (Skip) (Repeat stmt cond) : stmts) dict input  -- If the condition is met, we skip and drop the repeat. Otherwise, we queue up another repeat for the rest of the statements





instance Parse Statement where
  parse =
    assignment
    !  skip_
    !  secuencial_
    !  if_
    !  while_
    !  read_
    !  write_
    !  repeat_


  toString :: Statement -> String
  toString (Assignment v e) = v ++ ":=" ++ Expr.toString e ++ ";\n"
  toString (Skip) = "skip;\n"  
  toString (Sequencial s) = "begin\n" ++ concatMap toString s ++ "end\n"
  toString (If c t e) = "if " ++ Expr.toString c ++ " then\n" ++ toString t ++ "else\n" ++ toString e
  toString (While c body) = "while " ++ Expr.toString c ++ " do\n" ++ toString body
  toString (Read v) = "read " ++ v ++ ";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"
  toString (Repeat s c) = "repeat " ++ Expr.toString s ++ " until: " ++ Expr.toString c