-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)

    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const (10*n+(ord y - 48)), ys)

    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)


    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys

    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys

    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App f e) = f ++ "(" ++ unparse e ++ ")" -- This line is needed to be added in order to unparse App functions


-- test with (eval (parse "x*x+2*x*y+y*y") [("x",1),("y",2)])
-- test with (unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))"))))
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" e) env = sin (eval e env)
eval (App "cos" e) env = cos (eval e env)
eval (App "log" e) env = log (eval e env)
eval (App "exp" e) env = exp (eval e env)


-- test with:
-- let e1 = parse "x*x+2"
-- unparse e1
-- let d1 = diff (Var "x") e1
-- unparse d1

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" e) =
  Op "*" (App "cos" e) (diff v e)
diff v (App "cos" e) =
  Op "*" (Op "-" (Const 0) (App "sin" e)) (diff v e)
diff v (App "log" e) =
  Op "/" (diff v e) e
diff v (App "exp" e) =
  Op "*" (App "exp" e) (diff v e)


diff _ _ = error "can not compute the derivative"


-- test with:
-- let e1 = parse "x*x+2"
-- unparse e1
-- let d1 = diff (Var "x") e1
-- unparse d1
-- unparse (simplify d1)
simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App f e) =
  App f (simplify e)


-- test this with:
-- let f = mkfun (parse "x*x+2", Var "x")
-- f 3.0

-- in this case, 'parse "x*x+2"' == body, 'Var "x"' == vname, and our future input value to the function == val
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, Var vname) val = eval body [(vname, val)]


-- test with (findzero "x" "x*x*x+x-1" 1.0)
-- where "x" == varName, "x*x*x+x-1" == bodyStr, and 1.0 == x0 which is our initial value for x

findzero :: String -> String -> Float -> Float          
findzero varName bodyStr x0 =
  let
    expr = parse bodyStr -- Parse function body

    dexpr = diff (Var varName) expr -- Differentiate function body

    tolerance = 1.0e-6
    maxIter   = 1000000

    newton :: Int -> Float -> Float
    newton n x
      | n > maxIter = error "Exceeded max number of iterations."
      | abs fx < tolerance = x
      | otherwise          = newton (n+1) (x - fx / fpx)
      where
        env = [(varName, x)]  
        fx  = eval expr env   -- f(x)
        fpx = eval dexpr env  -- f'(x)

  in
    newton 0 x0
