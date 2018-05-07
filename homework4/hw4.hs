-- CS 456 Homework 4
-- Due date: 04/06/2018 by 9:00PM

-- Name: Vritant Bhardwaj
-- Email: bhardwav@purdue.edu

-- ASSIGNMENT INSTRUCTIONS:
-- ========================================================================
-- All questions have to be completed. Before submitting, make sure
-- that running 'ghc hw3.hs' in the shell completes without errors.
-- If your homework does not compile, it will not be graded! If an
-- incomplete answer to a question is causing ghc to fail, comment it
-- out.

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- Submit all of your files via turnin using the instructions here (windows
-- users will perform a similar incantation):
-- https://www.cs.purdue.edu/homes/cs177/turnin-from-home-macos.html
-- Use cs456 for the class name and hw4 for the project name:
-- ssh <your username>@lore.cs.purdue.edu /usr/local/bin/turnin -c cs456 -p hw4 <your files>

-- Part 1: Online Partial Evaluation

-- We saw in class how to partially evaluate a simple language with
-- integers, booleans, and functions. Note that the partial evaluator
-- does not fail gracefully: if, for example, a branch of an if is an
-- ill-typed expression like (1 + True), the partial evaluator will
-- fail, instead of returning a residual program. This is problematic
-- in the case, for example, when specializing a program like:
-- (If (x == 1) Then 0 Else head [ ])
-- When x is dynamic, the partial evaluator will throw an error, even
-- though the residual program should be fine as long as x maps to 1 in
-- the store.

-- In this part of the homework, we will ensure that the partial
-- evaluator always returns a residual program by inserting special
-- error values whenever an error is encountered at specialization
-- time.  We will furthermore enrich the language with list values and
-- let expressions.

fromJust :: Maybe a -> a
fromJust Nothing  = error "fromJust: Nothing"
fromJust (Just x) = x

type Prog = ([FDef], Expr)

type FDef = (String,([String],Expr))

-- Question 1: Add a construtor for list (of integer) values: [Int],
-- and for error values (where the errors constructor takes a single
-- String argument).
data Val =
    IVal { getInt :: Int }
  | BVal { getBool :: Bool }
  | LVal { getList :: [Int]}
  | ErrorVal {getError :: String}

-- Question 2: The operations over lists are:
-- 1) get the head of a list (Head),
-- 2) get the tail of a list (Tail),
-- 3) check if a list is empty (IsNil),
-- 4) add a new element to the head of a list value (Cons)
-- The Op datatype represents syntax for operations, extend
-- it with constructors for these three operations over lists.

data Op = Equal | Add | Sub | Mul | Head | Tail | IsNil | Cons

-- Question 3: Add syntax for let expressions:
-- let x := e1 in e2, where x is a string identifier.
data Expr =
   Const { getVal :: Val }
 | Var String
 | Apply String [Expr]
 | Prim Op [Expr]
 | If Expr Expr Expr
 | Let String Expr Expr

type Env = [(String, Val)]

-- Question 4: Extend the Show typeclasses to support all of our new
-- operations.
instance Show Expr where
 show (Const v) = show v
 show (Var s) = s
 show (Apply fun args) = fun ++ show args
 show (Prim op [a,b]) = show a ++ " " ++ show op ++ " " ++ show b
 show (Prim op [Const xs]) = show op ++ "(" ++ show xs ++ ")"
 show (If c a b) = "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
 show (Let x a b) = "Let " ++ show x ++ " = " ++ show a ++ " in " ++ show b

instance Show Val
 where
  show (IVal i) = show i
  show (BVal b) = show b
  show (LVal l) = show l
  show (ErrorVal e) = show e

instance Show Op where
  show Equal = "=="
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Head = "head"
  show Tail = "tail"
  show isNil = "isNil"

-- Question 5: Extend the evaluator for primitive operations to handle
-- the four you added above. The rules for these are:
-- 1) Head (x : xs) ---> x
-- 2) Tail (x : xs) ---> xs
-- 3) IsNil (x : xs) ---> false
--    isNil [ ] ---> False
-- 4) Cons x xs --> (x : xs)

eval_op Equal [IVal i1, IVal i2] = BVal (i1 == i2)
eval_op Add [IVal i1, IVal i2] = IVal (i1 + i2)
eval_op Sub [IVal i1, IVal i2] = IVal (i1 - i2)
eval_op Mul [IVal i1, IVal i2] = IVal (i1 * i2)
eval_op Head [LVal l1] = IVal (head l1)
eval_op Tail [LVal l1] = LVal (tail l1)
eval_op IsNil [LVal []] = BVal (True)
eval_op IsNil [LVal _] = BVal (False)
eval_op Cons [IVal x, LVal xs] = LVal (x:xs)

-- Question 6: Add an evaluation rule for let expressions:

--    σ, e1 ⇓ v1     σ [x --> v1], e2 ⇓ v2
--    ------------------------------ 	(E_Let)
--    σ, let x := e1 in e2 ⇓ v2

eval :: Prog -> Val
eval (fdefs, main) = eval' main [] where
  eval' :: Expr -> Env -> Val
  eval' (Const v) env = v
  eval' (Var s) env =
    case lookup s env of
      Just v -> v
      Nothing -> error "undefined variable"
  eval' (Prim op es) env =
    let rs = [ eval' e env | e <- es ] in
      eval_op op rs
  eval' (If e0 e1 e2) env =
    if getBool (eval' e0 env)
    then eval' e1 env
    else eval' e2 env
  eval' (Apply f es) env =
    eval' body env'
    where
      (ss, body) = fromJust (lookup f fdefs)
      env' = zip ss [ eval' e env | e <- es ]
  eval' (Let x e1 e2) env = 
    eval' e2 (env ++ [(x, e1')])
    where
      e1' = eval' e1 env

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

isVal :: Expr -> Bool
isVal (Const _) = True
isVal _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- Question 7: Extend the partial evaluator for primitive operators to
-- emit an Error value when an operator is applied to the wrong number
-- or type of arguments, as well as when head and tail are applied to
-- a nil list. (This is worth the equivalent of 5 'normal' questions).
peval_op Equal [IVal i1, IVal i2] = BVal (i1 == i2)
peval_op Equal _ = ErrorVal "incorrect use of Equal op"

peval_op Add [IVal i1, IVal i2] = IVal (i1 + i2)
peval_op Add _ = ErrorVal "incorrect use of Add op"

peval_op Sub [IVal i1, IVal i2] = IVal (i1 - i2)
peval_op Sub _ = ErrorVal "incorrect use of Sub op"

peval_op Mul [IVal i1, IVal i2] = IVal (i1 * i2)
peval_op Mul _ = ErrorVal "incorrect use of Mul op"

peval_op Head [LVal []] = ErrorVal "Head op cannot be used on empty list"
peval_op Head [LVal l1] = IVal (head l1)
peval_op Head _ = ErrorVal "incorrect use of Head op"

peval_op Tail [LVal []] = ErrorVal "Tail op cannot be used on empty list"
peval_op Tail [LVal l1] = LVal (tail l1)
peval_op Tail _ = ErrorVal "incorrect use of Tail op"

peval_op IsNil [LVal []] = BVal True
peval_op IsNil [LVal _] = BVal False
peval_op IsNil _ = ErrorVal "incorrect use of IsNil op"

peval_op Cons [IVal x, LVal xs] = LVal (x:xs)
peval_op Cons _ = ErrorVal "incorrect use of Cons op"

-- Question 8: Augment the partial evaluator to handle let
-- expressions: If the expression bound by the let partially evaluates
-- to a value, it can be substituted in the body of the let expression
-- and the result can be partially evaluated. If not, we'll
-- residualize the let expression, after partially evaluating the
-- bound expression and the body of the let. (This is worth the equivalent of 5 'normal' questions).

-- Γ, σ; e1 ⇂ Γ1; v1      Γ1; σ[x --> v1]; e2 ⇂ Γ2; e2'
-- ------------------------------------------------------ PE-SLET
--Γ, σ; let x := e1 in e2 ⇂ Γ2; e2'

-- Γ, σ; e1 ⇂ Γ1; e1'      Γ1; σ; e2 ⇂ Γ2; e2'
-- ------------------------------------------------------ PE-DLET
--Γ, σ; let x := e1 in e2 ⇂ Γ2; let x := e1' in e2'

peval :: Prog -> Prog
peval (fdefs, main) = swap (runState (peval' main []) [])
  where
    peval' :: Expr -> Env -> State [FDef] Expr
    peval' (Const v) env = return (Const v)
    peval' (Var s) env =
      case lookup s env of
        Just v -> return (Const v)
        Nothing -> return (Var s)

    peval' (Prim op es) env = do
      rs <- mapM (flip peval' env) es
      if all isVal rs
      then return (Const (peval_op op (map getVal rs)))
      else return (Prim op rs)

    peval' (If e0 e1 e2) env = do
      r0 <- peval' e0 env
      if isVal r0
      then if getBool (getVal r0)
           then peval' e1 env
           else peval' e2 env
      else do
           r1 <- peval' e1 env
           r2 <- peval' e2 env
           return (If r0 r1 r2)

    peval' (Apply s es) env = do
    -- Look up function.
      let (ss, body) = fromJust (lookup s fdefs)

      -- Partially evaluate arguments.
      rs <- mapM (flip peval' env) es

      -- Determine static and dynamic arguments.
      let z = zip ss rs
          sas = [ (s, getVal r) | (s, r) <- z, isVal r ]
          das = [ (s,v) | (s,v) <- z, not (isVal v) ]
      if null das
      -- Inline completely static applications.
      then peval' body sas

      -- Otherwise these applications make their contexts dynamic.
      else do

      -- Fabricate name from static variables.
        let s' = s ++ show (show sas)

        -- Specialize each "name" just once.
        fdefs <- get
        when (isNothing (lookup s' fdefs)) (do

        -- Create placeholder for memoization.
          put (fdefs ++ [(s', undefined)])

          -- Partially evaluate function body.
          e' <- peval' body sas

          -- Replace placeholder by actual definition.
          modify (update (const (map fst das, e')) s'))

        -- Return application of specialized function.
        return (Apply s' (map snd das))

    peval' (Let x e1 e2) env = do
      e1' <- peval' e1 env
      e2' <- peval' e2 env
      if isVal e1'
        then peval' e2 (env ++ [(x, (getVal e1'))])
        else return $ Let x e1' e2'

update :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
update f k ((k',v):kvs) = if k==k' then (k',f v):kvs else (k',v):update f k kvs

-- Example program for testing:
expF = (
 "exp", (["x","n"],
  If (Prim Equal [Var "n", Const (IVal 0)])
     (Const (IVal 1))
     (Prim Mul
       [Var "x",
        Apply "exp" [Var "x", Prim Sub [Var "n", Const (IVal 1)]]])))

expProg = ([expF], Apply "exp" [Const (IVal 2), Const (IVal 3)])

-- Questions 9-11: Write three example programs demonstrating using the
-- features you have added to the language: one using let, one using
-- list values and operations, and another which will produce a
-- residual program which includes an Error.

expProgLet = ([], Let "x" (Const (BVal False)) (If (Var "x") (Const (IVal 1)) (Const (IVal 2))))
expProgList  = ([], Prim Tail [Const (LVal [1,2,3])])
expProgError  = ([], Prim Head [Const (LVal [])])

printBind (name, (args, body)) = do
    putStr "  "
    putStr name
    print args
    putStr "    "
    print $ body

-- test partially evaluates the expression exp in environment env.
test (env,exp) = do
  putStr "\n> "
  print $ exp
  sequence [ printBind bnd | bnd <- env ]
  putStr "* "
  let (env',exp') = peval (env,exp)
  print exp'
  sequence [ printBind bnd | bnd <- env' ]

testexps = do
      test expProgLet
      test expProgList
      test expProgError

-- Part 2: Type Checking

-- In this section, we'll be using the type system for Imp we saw in
-- class to build an executable type-checker for Imp programs.

-- Syntax for expressions:
data Exp =
       Variable String
     | ConstExp Int
     | Plus Exp Exp
     | Minus Exp Exp
     | BTrue
     | BFalse
     | BNot Exp
     | BAnd Exp Exp
     | BEq Exp Exp
     | BLt Exp Exp
     | BIf Exp Exp Exp
     deriving Show

-- Syntax for statements
data Stmt =
       Assign String Exp
     | Skip
     | Seq Stmt Stmt
     | IfThenElse Exp Stmt Stmt
     | While Exp Stmt
     deriving Show

-- Arithmetic expressions either have type Int or type Bool
data ExpType =
       IntType
     | BoolType deriving (Show, Eq)

-- Imp statements have a single type, Comm
data StmtType = CommType deriving Show

-- Typing Contexts map variable names to types
type TypCtxt = [(String, ExpType)]

-- Extending an typing context with a new variable typing
extend :: String -> ExpType -> TypCtxt -> TypCtxt
extend x ty env = (x, ty) : env

-- Our checker will throw different kinds of type errors
data ExpTypeError
  = Mismatch ExpType ExpType
  | BadAssign String ExpType ExpType
  | NotFunction ExpType
  | NotInScope String deriving Show

-- The typechecker will use a combination of the Exception and Reader
-- monads to keep track of type errors and variable types, respectively.

type CheckM = ExceptT ExpTypeError (Reader TypCtxt) 

inTypCtxt :: String -> ExpType -> CheckM a -> CheckM a
inTypCtxt x ty = local (extend x ty)

lookupVar :: String -> CheckM ExpType
lookupVar x = do
  env <- ask -- Get the current typing context
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

-- Questions 12-18: Write a typechecker for expressions, following the
-- typing rules included in the comments above each case. You can use
-- the lookupVar and extend functions defined above, as
-- necessary. Make sure to return an appropriate type error in the
-- case that a rule does not apply.
typeCheckExp :: Exp -> CheckM ExpType

--    Γ(x) = T
--  ------------  T-Var
--    Γ ⊢ x : T
typeCheckExp (Variable x) = lookupVar x


--
--  ------------  T-Const
--    Γ ⊢ n : Int
typeCheckExp (ConstExp n) = return IntType

--   Γ ⊢ e1  : Int     Γ ⊢ e2  : Int
--  ----------------------------------  T-Plus
--            Γ ⊢ e1 + e2 : Int
typeCheckExp (Plus e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  case t1 of
    IntType -> case t2 of 
      IntType -> return IntType
      _ -> throwError $ Mismatch t1 t2
    _ -> throwError $ Mismatch t1 t2


--   Γ ⊢ e1  : Int     Γ ⊢ e2  : Int
--  ----------------------------------  T-Plus
--            Γ ⊢ e1 - e2 : Int
typeCheckExp (Minus e1 e2) = typeCheckExp (Plus e1 e2)

--
--  ------------  T-True
--    Γ ⊢ true : Bool
typeCheckExp (BTrue) = return BoolType

--
--  ------------  T-False
--    Γ ⊢ false : Bool
typeCheckExp (BFalse) = return BoolType

--   Γ ⊢ e : Bool
--  ------------  T-Not
--    Γ ⊢ not e : Bool
typeCheckExp (BNot e) = do
  t1 <- typeCheckExp e 
  case t1 of
    BoolType -> return BoolType
    _ -> throwError $ Mismatch t1 BoolType

-- Questions 19-22: For the remaining four kinds of expressions, formulate
-- and write an appropriate typing rule in a comment above each case
-- as above, in addition to writing the implementation of typeCheckExp
-- for each case.

--   Γ ⊢ e1  : Bool     Γ ⊢ e2  : Bool
--  ----------------------------------  T-And
--            Γ ⊢ e1 && e2 : Bool
typeCheckExp (BAnd e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  case t1 of
    BoolType -> case t2 of 
      BoolType -> return BoolType
      _ -> throwError $ Mismatch t1 t2
    _ -> throwError $ Mismatch t1 t2

--   Γ ⊢ e1  : T     Γ ⊢ e2  : T
--  ----------------------------------  T-Eq
--            Γ ⊢ e1 == e2 : Bool
typeCheckExp (BEq e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  if t1 == t2 
    then return BoolType
    else throwError $ Mismatch t1 t2

--   Γ ⊢ e1  : Int     Γ ⊢ e2  : Int
--  ----------------------------------  T-Lt
--            Γ ⊢ e1 < e2 : Bool
typeCheckExp (BLt e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  case t1 of
    (IntType) -> case t2 of
      (IntType) -> return BoolType
      _ -> throwError $ Mismatch t2 IntType
    _ -> throwError $ Mismatch t1 IntType

--   Γ ⊢ e1  : Bool     Γ ⊢ e2  : T     Γ ⊢ e3  : T
--  ----------------------------------------------- T-If
--           Γ ⊢ if e1 then e2 else e3 : T
typeCheckExp (BIf ec et ee) = do
  t1 <- typeCheckExp ec
  t2 <- typeCheckExp et
  t3 <- typeCheckExp ee

  case t1 of
    BoolType -> case t2 of 
      IntType -> case t3 of
        IntType -> return IntType
        _ -> throwError $ Mismatch t2 t3
      BoolType -> case t3 of
        BoolType -> return BoolType
        _ -> throwError $ Mismatch t2 t3
    _ -> throwError $ Mismatch t1 BoolType

-- Question 22-26: Write a typechecker for Imp statements, following the
-- typing rules included in the comments above each case. Make sure to
-- return an appropriate type error in the case that a rule does not
-- apply.

typeCheckStmt :: Stmt -> CheckM StmtType

--   Γ ⊢ e : T   Γ(x) = T
--  ----------------------  T-Assign
--    Γ ⊢ x := e : Comm
typeCheckStmt (Assign x e) = do
  te <- typeCheckExp e
  tx <- typeCheckExp (Variable x)
  case te of
    IntType -> case tx of
      IntType -> return CommType
      _ -> throwError $ BadAssign x tx te
    BoolType -> case tx of
      BoolType -> return CommType
      _ -> throwError $ BadAssign x tx te


--
--  ----------------------  T-Skip
--    Γ ⊢ skip : Comm
typeCheckStmt Skip = return CommType

--   Γ ⊢ s1 : Comm    Γ ⊢ s2 : Comm
--  ---------------------------------   T-Seq
--    Γ ⊢ s1; s2 : Comm
typeCheckStmt (Seq s1 s2) = do
  t1 <- typeCheckStmt s1
  t2 <- typeCheckStmt s2
  return CommType

--   Γ ⊢ ec : Bool    Γ ⊢ st : Comm   Γ ⊢ se : Comm
--  ------------------------------------------------  T-If
--    Γ ⊢ if ec then st else sc : Comm
typeCheckStmt (IfThenElse ec st sc) = do
  tc <- typeCheckExp ec
  case tc of
    BoolType -> do
      tt <- typeCheckStmt st
      tc <- typeCheckStmt sc
      return CommType
    _ -> throwError $ Mismatch tc BoolType

--   Γ ⊢ e : Bool    Γ ⊢ s : Comm
--  ------------------------------  T-While
--    Γ ⊢ while e do { s } : Comm
typeCheckStmt (While e s) = do
  te <- typeCheckExp e
  case te of
    BoolType -> do
      ts <- typeCheckStmt s
      return CommType
    _ -> throwError $ Mismatch te BoolType

runCheck :: TypCtxt -> CheckM a -> Either ExpTypeError a
runCheck env = flip runReader env . runExceptT

-- Question 27-30: Write two examples of well-formed Imp statements
-- involving either while or seq, and two examples of ill-formed Imp
-- statements (provide appropriate typing contexts to make an
-- expression well or ill-formed.

goodTypCtxt1 = [("x", BoolType)]
goodImpStmt1 = While (BEq (Variable "x") BTrue) (Assign "x" BFalse)
res1 = runCheck goodTypCtxt1 (typeCheckStmt goodImpStmt1)

goodTypCtxt2 = [("x", IntType)]
goodImpStmt2 = Seq (Assign "x" (ConstExp 1)) (Skip)
res2 = runCheck goodTypCtxt2 (typeCheckStmt goodImpStmt2)

badTypCtxt1 = [("x", BoolType)]
badImpStmt1 = Seq (Assign "x" (ConstExp 1)) (Skip)
res3 = runCheck badTypCtxt1 (typeCheckStmt badImpStmt1)

badTypCtxt2 = [("x", IntType)]
badImpStmt2 = While (BEq (Variable "x") BTrue) (Assign "x" BFalse)
res4 = runCheck badTypCtxt2 (typeCheckStmt badImpStmt2)

-- Part 3: Type Inference for the Lambda Calculus
data SimpleType =
       TBool
     | TyArrow SimpleType SimpleType
     | TyVar Int
      deriving (Eq)

instance Show SimpleType where
     show (TyVar n) = "X" ++ show n
     show TBool = "Bool"
     show (TyArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- The full syntax of this language is thus:
--        t ::= x                      variable
--           | \x:T. t2                abstraction
--           | t1 t2                   application
--           | true                    constant true
--           | false                   constant false
--           | if t1 then t2 else t3   conditional

data STLCExp =                        -- t ::=
     STVar String                     --       x
   | Lambda String SimpleType STLCExp --       \x : T. t
   | App STLCExp STLCExp              --       t1 t2
   | BoolExp Bool                     --       true | false
   | IfExp STLCExp STLCExp STLCExp    --       if tc then tt else te
   deriving (Eq)

instance Show STLCExp where
  show (STVar x) = x
  show (Lambda x ty e) = "\\" ++ x ++ " : " ++ show ty ++ ". " ++ show e
  show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (BoolExp True) = "true"
  show (BoolExp False) = "false"
  show (IfExp ec et ee) = "if " ++ show ec ++ " then " ++ show et ++ " else " ++ show ee

-- Environments map variable names to types
type LamTypCtxt = [(String, SimpleType)]

-- Extend ing an Environment with a new variable typing
extendLTC :: String -> SimpleType -> LamTypCtxt -> LamTypCtxt
extendLTC x ty env = (x, ty) : env

-- Type Constraints are equations between types: T1 = T2
data TypeConstraint = TypeEq SimpleType SimpleType
instance Show TypeConstraint where
         show (TypeEq t1 t2) = show t1 ++ " = " ++ show t2
type TypeConstraints = [TypeConstraint]

-- Our checker only fails if there is an occurence of an unbound variable
data SimpleTypeError
  = UnBoundVar String deriving Show

-- The constaint-based type-checker has monads in order to:
-- Throw an error if there is an unbound type variable (ExceptT SimpleTypeError)
-- Keep track of the current typing context (ReaderT LamTypCtxt)
-- and Keep a list of fresh type variables (State Int)
type InferM = ExceptT SimpleTypeError (ReaderT LamTypCtxt (State Int))

-- inLamTypCtxt x ty e evaluates e in the current context extended with [x --> ty]
inLamTypCtxt :: String -> SimpleType -> InferM a -> InferM a
inLamTypCtxt x ty = local (extendLTC x ty)

-- getFresh gets a fresh variable in the current context
getFresh :: InferM Int
getFresh = do
           x <- get
           y <- put (x + 1)
           return x

-- lookupSTVar gets the type of a variable in the current typing context
lookupSTVar :: String -> InferM SimpleType
lookupSTVar x = do
  env <- ask -- Get the current environment
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ UnBoundVar x

-- Question 31-35: Write a constraint-based typechecker for Imp
-- statements, following the typing rules included in the comments
-- above each case. Make sure to return an appropriate type error in
-- the case that a rule does not apply.
inferConstraints :: STLCExp -> InferM (SimpleType, TypeConstraints)


--  ----------------------  CT-True
--    Γ ⊢ true : Bool | [ ]

--  ----------------------  CT-False
--    Γ ⊢ false : Bool | [ ]
inferConstraints (BoolExp _) = return (TBool, [])

--   Γ ⊢ ec : Tc | Cc     Γ ⊢ et : Tt | Ct     Γ ⊢ ee : Te | Ce
--  ----------------------  CT-If
--    Γ ⊢ if ec then et else ee : Tt | [Tc = Bool, Tt = Te] ++ Cc ++ Ct ++ Ce
inferConstraints (IfExp ec et ee) = do
  (tc, cc) <- inferConstraints ec 
  (tt, ct) <- inferConstraints et
  (te, ce) <- inferConstraints ee
  return (tt, [(TypeEq tc TBool), (TypeEq tt te)] ++ cc ++ ct ++ ce)

--   Γ [x --> ty] ⊢ e : ty2 | C
--  ------------------------------  CT-Abs
--    Γ ⊢ \x:ty. e : ty -> ty2 | C
inferConstraints (Lambda x ty e) = do
  (ty2, c) <- inLamTypCtxt x ty (inferConstraints e)
  return (TyArrow ty ty2, c)


--   Γ ⊢ e1 : ty1 | C1          Γ ⊢ e2 : ty2 | C2            fresh X
--  -----------------------------------------------------------------  CT-Abs
--    Γ ⊢ e1 e2 : X | [ty1 = ty2 -> X] ++ C1 ++ C2

inferConstraints (App e1 e2) = do
  (ty1, c1) <- inferConstraints e1
  (ty2, c2) <- inferConstraints e2
  f <- getFresh
  return (TyVar f, [(TypeEq ty1 (TyArrow ty2 (TyVar f)))] ++ c1 ++ c2)

--    Γ(x) = T
--  -------------- CT-Var
--    Γ ⊢ x : T
inferConstraints (STVar x) = do
  ty <- lookupSTVar x
  return (ty, [])

-- runInfer takes the starting value of fresh variables, a typing
-- context, and an expression to do constraint-based typing on
runInferConstraints :: Int -> LamTypCtxt -> STLCExp -> Either SimpleTypeError (SimpleType, TypeConstraints)
runInferConstraints freshestVar gamma e = fst (flip runState freshestVar (flip runReaderT gamma (runExceptT (inferConstraints e))))

lamExp1 = Lambda "x" (TyVar 0) (Lambda "y" (TyVar 1) (Lambda "z" (TyVar 2) (App (App (STVar "x") (STVar "z")) (App (STVar "y") (STVar "z")))))
-- print $ runInferConstraints 3 [ ] lamExp1 should equal ((X0 -> (X1 -> (X2 -> X5))),[X3 = (X4 -> X5),X0 = (X2 -> X3),X1 = (X2 -> X4)])

lamExp2 = Lambda "x" (TyVar 0) (App (STVar "x") (STVar "x"))
-- print $ runInferConstraints 3 [ ] lamExp2 should equal ((X0 -> X3),[X0 = (X0 -> X3)])

-- Substitutions are maps from Type Variables to Types:
type TypeSubst = [(Int, SimpleType)]

-- Questions 36-37: Implememnt the type substitution application and type
-- substitution functions discussed in class.

getVar :: TypeSubst -> Int -> Maybe SimpleType
getVar [] _ = Nothing
getVar ((v, ty):xs) n = if v == n then (Just ty) else (getVar xs n)

applyTypeSubst :: TypeSubst -> SimpleType -> SimpleType
applyTypeSubst sigma (TBool) = TBool
applyTypeSubst sigma (TyArrow t1 t2) =  TyArrow (applyTypeSubst sigma t1) (applyTypeSubst sigma t2)
applyTypeSubst sigma (TyVar n) = 
  case getVar sigma n of
    (Just ty) -> ty
    (Nothing) -> TyVar n


filterTS :: TypeSubst -> TypeSubst -> TypeSubst
filterTS [] ys = []
filterTS ((v, ty):xs) ys = 
  case getVar ys v of
    (Nothing) -> (v, ty):(filterTS xs ys)
    (Just ty) -> filterTS xs ys

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst sigma1 sigma2 = 
  map (
    \(v, ty) -> 
      case getVar sigma2 v of
        (Nothing) -> (v, applyTypeSubst sigma1 (TyVar v))
        (Just ty) -> (v, applyTypeSubst sigma1 ty)
  ) ((filterTS sigma1 sigma2) ++ sigma2)

-- composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
-- composeTypeSubst sigma1 sigma2 = (filterTS sigma1 sigma2) ++ sigma2

composeRes = composeTypeSubst [(1, TyVar 2), (3, TyVar 4)] [(5, TyArrow TBool TBool), (2, TyVar 3)] 


isFreeVar :: Int -> SimpleType -> Bool
isFreeVar n (TyVar x) = not (x == n)
isFreeVar n (TBool) = True
isFreeVar n (TyArrow t1 t2) = (isFreeVar n t1) && (isFreeVar n t2)

caseEqT :: SimpleType -> Bool -> Bool
caseEqT (TyVar x) True = True
caseEqT _ _ = False

caseArrow :: SimpleType -> SimpleType -> Bool
caseArrow (TyArrow _ _) (TyArrow _ _) = True
caseArrow _ _ = False

getFA :: SimpleType -> SimpleType
getFA (TyArrow t1 _) = t1

getSA :: SimpleType -> SimpleType
getSA (TyArrow _ t2) = t2

getV :: SimpleType -> Int
getV (TyVar x) = x

unify :: TypeConstraints -> Except () TypeSubst
unify [] = return []
unify ((TypeEq s t):c') = 
  if s == t
    then do
      res <- unify c'
      return res
    else if caseEqT s (isFreeVar (getV s) t) 
        then do
          res <- unify $ map (
              \(TypeEq a b) -> (TypeEq (applyTypeSubst [(getV s, t)] a) (applyTypeSubst [(getV s, t)] b))
            ) c' 
          return $ composeTypeSubst res [(getV s, t)]
    else if caseEqT t (isFreeVar (getV t) s)
        then do
          res <- unify $ map (
              \(TypeEq a b) -> (TypeEq (applyTypeSubst [(getV t, s)] a) (applyTypeSubst [(getV t, s)] b))
            ) c' 
          return $ composeTypeSubst res [(getV t, s)]
    else if caseArrow s t
      then do
        res <- unify (c' ++ [(TypeEq (getFA s) (getFA t)), (TypeEq (getSA s) (getSA t))])
        return res
    else throwError ()

-- Question 39: Combine your answers to Question 13 with
-- runInferConstraints to build a type inference function that takes a
-- simply-typed lambda term, e, and returns the principal type of e in
-- the empty typing context.  Note: you'll need to give
-- runInferConstraints an appropriately large value for fresh
-- variables. (This question is worth the equivalent of 3 "normal"
-- questions.)

inferType :: STLCExp -> Maybe SimpleType
inferType exp = case runInferConstraints 10 [] exp of
  (Left x) -> Nothing
  (Right (st, tcs)) -> case runExcept $ unify tcs of
    (Left _ ) -> Nothing
    (Right res) -> return $ applyTypeSubst res st


-- inferType lamExp1 should evaluate to: Just ((X2 -> (X11 -> X12)) -> ((X2 -> X11) -> (X2 -> X12)))
-- inferType lamExp2 should evaluate to: Nothing

lamExp3 = Lambda "x" (TyVar 0) (Lambda "x" (TBool) (BoolExp True))

main = print $ "Does anyone read this line? yes"
