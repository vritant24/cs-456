-- CS 456 Homework 3
-- Due date: 03/06/2018 by 9:00PM

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
import Control.Monad.Identity

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- Submit all of your files via turnin using the instructions here (windows
-- users will perform a similar incantation):
-- https://www.cs.purdue.edu/homes/cs177/turnin-from-home-macos.html
-- Use cs456 for the class name and hw3 for the project name:
-- ssh <your username>@lore.cs.purdue.edu /usr/local/bin/turnin -c cs456 -p hw3 <your files>

-- Part 1: Lambda Calculus with Booleans

-- The lambda calculus we saw in class was a core calculus consisting
-- of function abstraction, function application, and variables. While
-- this calculus is Turing complete, it isn't so great to program in,
-- so people often study extensions of the calculus that make it more
-- familiar. One simple extension is the addition of booleans to the
-- calculus, via syntax for true, false, and conditionals.

-- The full syntax of this language is thus:
--        t ::= x                      variable
--           | \x. t2                  abstraction
--           | t1 t2                   application
--           | true                    constant true
--           | false                   constant false
--           | if t1 then t2 else t3   conditional

-- Recall the CBV small-step semantics of the core lambda calculus
-- from class :

--       value v2
--    ------------ (ST_AppAbs)
--   (\x:T.t12) v2 ==> [x:=v2]t12
--
--      t1 ==> t1'
--    ------------  (ST_App1)
--   t1 t2 ==> t1' t2
--
--    value v1         t2 ==> t2'
--    --------------------------- (ST_App2)
--    v1 t2 ==> v1 t2'

-- Our definition of values needs to be extended to account for the
-- new expressions, so you can now read "value v" as v is either a
-- lambda abstraction, or true, or false.

-- Question 1: Define CBV small-step reduction rules for the
-- lambda+boolean calculus by writing reduction rules for the new
-- expressions, using the examples above as a formatting
-- template. (Three rules should suffice).

--       
--    ------------------------------ (ST_condTrue)
--   if true then t1 else t2 ==> t1

--       
--    ------------------------------ (ST_condFalse)
--   if false then t1 else t2 ==> t2

--       t1 ==> t1'
--    ------------------------------ (ST_condReduce)
--   if t1 then t2 else t3 ==> if t1' then t2 else t3

-- Questions 2-4: Using your new rules and the CBV rules, show how to
-- reduce the following expressions to a normal form (i.e. a term that
-- cannot be reduced any further). As an example,
-- ((\x. x) (\x. x)) true
-- ==> (\x. x) true
-- ==> true

-- Question 2: (\x.x) ((\y. y) (\z.z)) 
--             ==> (\x.x) (\z.z)
--             ==> (\z.z)

-- Question 3: ((\x. x) (\y. if y then false else true)) true 
--             ==> (\y. if y then false else true) true
--             ==> if true then false else true
--             ==> false

-- Question 4: (\x. (\y. if ((\z. z) y) then (if x then false else true) else x)) true false
--             ==> (\y. if ((\z. z) y) then (if true then false else true) else true) false
--             ==> if ((\z. z) false) then (if true then false else true) else true)
--             ==> if false then (if true then false else true) else true
--             ==> true

-- Bonus Question: What standard boolean functions do Questions 3 and
-- 4 compute?
-- 3 computes the NOT operation
-- 4 computes the XOR operation

-- Questions 5-7: Using your new rules and the CBN rules, show how to
-- reduce the following expressions to a normal form (i.e. a term that
-- cannot be reduced any further).

-- Question 5: (\x.x) ((\y. y) (\z.z)) ==>
--             ==> (\y. y) (\z.z)
--             ==> (\z.z)

-- Question 6: ((\x. x) (\y. if y then false else true)) true ==>
--             ==> (\y. if y then false else true) true
--             ==> if true then false else true
--             ==> true

-- Question 7: (\x. (\y. if ((\z. z) y) then (if x then false else true) else x)) true false ==>
--             ==> (\y. if ((\z. z) y) then (if true then false else true) else true) false
--             ==> if ((\z. z) false) then (if true then false else true) else true
--             ==> if false then (if true then false else true) else true
--             ==> true

-- The first step to writing an interpreter for our new calculus is to
-- represent the abstract syntax trees for this calculus as an
-- algebraic data type in Haskell. Here is the data type for the
-- pure lambda calculus, using Strings for variable identifiers:

data Exp =              -- t ::=
    Var String         --       x
  | Lambda String Exp  --       \x. t
  | App Exp Exp        --       t1 t2
-- Question 8: Write constructors for the three kinds of boolean
-- expressions to build a data type for the syntax of the
-- lambda+boolean calculus.
  | TrueE              -- constant True
  | FalseE             -- constant False
  | If Exp Exp Exp     -- conditional
  deriving (Show)
  
-- Question 9: Encode [(\x.x) ((\y. y) (\z.z))] as a Exp
exp9 :: Exp
exp9 = App (Lambda "x" (Var "x")) (App (Lambda "y" (Var "y")) (Lambda "z" (Var "z")))

-- Question 10: Encode [((\x. x) (\y. if y then false else true)) true] as a Exp
exp10 :: Exp
exp10 = App (Lambda "x" (Var "x")) (App (Lambda "y" (If (Var "y") FalseE TrueE)) TrueE)

-- Question 11: Encode [(\x. (\y. if ((\z. z) y) then (if x then false else true) else x)) true false] as a Exp
exp11 :: Exp
exp11 = App 
          (Lambda "x"
            (App 
              (Lambda "y" 
                (If 
                  (App (Lambda "z" (Var "z")) (Var "y"))
                  (If (Var "x") FalseE TrueE)
                  (Var "x")
                )
              )
              TrueE 
            )
          )
          FalseE

-- Question 12: Encode omega (the forever-looping term we saw in class) as a Exp
omega :: Exp
omega = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

-- Question 13: Any occurrences of a variable in lambda abstraction in
-- its body are said to be "bound" by that abstraction*. As an
-- example, the [x] in the body of [\x. \y. y x z] is bound by the
-- first lambda abstraction.  Any variable in that is not bound is
-- said to be "free", the z in [\x. \y. y x z] is an example of a free
-- variable.  Write a function that checks whether a particular
-- variable appears free in a lambda expression.

appearsFreeIn :: Exp -> String -> Bool
appearsFreeIn (Var v)       var = if(v == var) then True else False
appearsFreeIn (Lambda v e)  var = if(v == var) then False else (appearsFreeIn e var)
appearsFreeIn (App e1 e2)   var = (appearsFreeIn e1 var) || (appearsFreeIn e2 var)
appearsFreeIn (If e1 e2 e3) var = (appearsFreeIn e1 var) || (appearsFreeIn e2 var) || (appearsFreeIn e3 var)
appearsFreeIn _             var = False

-- *(assuming there aren't any lambda abstractions in its body that
--   bind the same variable).

-- Question 14: Write a function that checks whether or not an [Exp]
-- is a value, per the definition above.
isValue :: Exp -> Bool
isValue TrueE = True
isValue FalseE = True
isValue (Lambda _ _) = True
isValue _ = False

-- Question 15: The "workhorse" of the operational semantics for the
-- lambda calculus is the substitution function used to replace a
-- variable with an expression when reducing an
-- application. Informally, this function is defined as:
-- [x:=s] x               = s
-- [x:=s] y               = y                      if x ≠ y
-- [x:=s] (\x. t)         = \x. t
-- [x:=s] (\y. t)         = \y. [x:=s]t      if x ≠ y
-- [x:=s] (t1 t2)         = ([x:=s]t1) ([x:=s]t2)
-- [x:=s] true            = true
-- [x:=s] false           = false
-- [x:=s] (if t1 then t2 else t3) =
--                 if [x:=s]t1 then [x:=s]t2 else [x:=s]t3
-- Define a substitution function for [Exp]:
substExp :: String -> Exp -> Exp -> Exp
substExp x s (App e1 e2)    = App (substExp x s e1) (substExp x s e2)
substExp x s (If e1 e2 e3)  = If (substExp x s e1) (substExp x s e2) (substExp x s e3)
substExp x s (Var v)        = if (v == x) then s else (Var v)
substExp x s (Lambda v e)   = if (v == x) then (Lambda v e) else (Lambda v (substExp x s e))
substExp _ _ e              = e

-- Question 16: At long last, we are equipped to define an interpreter
-- for the lambda+boolean calculus. Define a function that takes a
-- single step according to the CBV reduction rules + the rules you
-- defined in Question 1 if possible and returning Nothing otherwise.

evalOneStepCBV :: Exp -> Maybe Exp
evalOneStepCBV (Var x)        = return $ Var x
evalOneStepCBV (TrueE)        = return $ TrueE
evalOneStepCBV (FalseE)       = return $ FalseE
evalOneStepCBV (Lambda v e)   = return $ Lambda v e
evalOneStepCBV (App e1 e2)    = if(isValue e1)
                                then 
                                  if(isValue e2)
                                    then case e1 of
                                      (Lambda x e)  -> return $ substExp x e2 e1
                                      _             -> return $ App e1 e2
                                    else do
                                      e2' <- evalOneStepCBV e2
                                      return $ App e1 e2'
                                else do
                                  e1' <- evalOneStepCBV e1
                                  return $ App e1' e2
evalOneStepCBV (If e1 e2 e3)  = if (isValue e1)
                                then case e1 of
                                  (TrueE)   -> return e1
                                  (FalseE)  -> return e2
                                  _         -> return $ If e1 e2 e3
                                else evalOneStepCBV e1 >>= (\x -> return $ If x e2 e3)

-- Question 17: Of course, we are *also* equipped to define an CBN
-- interpreter for the lambda+boolean calculus according to the CBN
-- reduction rules + the rules you defined in Question 1. Do that too!

evalOneStepCBN :: Exp -> Maybe Exp
evalOneStepCBN (Var x)        = return $ Var x
evalOneStepCBN (TrueE)        = return $ TrueE
evalOneStepCBN (FalseE)       = return $ FalseE
evalOneStepCBN (Lambda v e)   = return $ Lambda v e
evalOneStepCBN (App e1 e2)    = if (isValue e1)
                                then case e1 of
                                  (Lambda x e)  -> return $ substExp x e2 e1
                                  _             -> return $ App e1 e2
                                else evalOneStepCBN e1 >>= (\x -> return $ App x e2)
evalOneStepCBN (If e1 e2 e3)  = if (isValue e1)
                                then case e1 of
                                  (TrueE)   -> return e1
                                  (FalseE)  -> return e2
                                  _         -> return $ If e1 e2 e3
                                else evalOneStepCBN e1 >>= (\x -> return $ If x e2 e3)

-- Questions 18+19: Define multistep interpreters for the
-- lambda+boolean calculus which take as an argument the maximum
-- number of reduction steps to take. If a term is in normal form, the
-- function return that term. You can validate these functions using
-- your answers to Questions 2-7.

evalMultiStepCBV :: Int -> Exp -> Maybe Exp
evalMultiStepCBV 0 e              = return e
evalMultiStepCBV _ (Var x)        = return $ Var x
evalMultiStepCBV _ (TrueE)        = return $ TrueE
evalMultiStepCBV _ (FalseE)       = return $ FalseE
evalMultiStepCBV i (Lambda s e)   = return $ Lambda s e
evalMultiStepCBV i (If e1 e2 e3)  = do
                                    e1' <- evalMultiStepCBV (i - 1) e1
                                    e2' <- evalMultiStepCBV (i - 1) e2
                                    e3' <- evalMultiStepCBV (i - 1) e3
                                    case e1' of
                                      (TrueE)   -> return e2'
                                      (FalseE)  -> return e3'
                                      _         -> Nothing
evalMultiStepCBV i (App e1 e2)    = do
                                      e1' <- evalMultiStepCBV (i - 1) e1
                                      e2' <- evalMultiStepCBV (i - 1) e2
                                      case e1' of
                                        (Lambda s e) -> if(isValue e2')
                                                          then return $ substExp s e2' e1'
                                                          else return $ App e1' e2'
                                        _ -> return $ App e1' e2'

evalMultiStepCBN :: Int -> Exp -> Maybe Exp
evalMultiStepCBN 0 e              = return e
evalMultiStepCBN _ (Var x)        = return $ Var x
evalMultiStepCBN _ (TrueE)        = return $ TrueE
evalMultiStepCBN _ (FalseE)       = return $ FalseE
evalMultiStepCBN _ (Lambda v e)   = return $ Lambda v e
evalMultiStepCBN i (If e1 e2 e3)  = do
                                      e1' <- evalMultiStepCBN (i - 1) e1
                                      e2' <- evalMultiStepCBN (i - 1) e2
                                      e3' <- evalMultiStepCBN (i - 1) e3
                                      case e1' of
                                        (TrueE)   -> return e2'
                                        (FalseE)  -> return e3'
                                        _         -> Nothing
evalMultiStepCBN i (App e1 e2)    = do
                                    e1' <- evalMultiStepCBN (i - 1) e1
                                    case e1' of
                                      (Lambda s e) -> return $ substExp s e2 e1'
                                      _            -> return $ App e1' e2

-- Question 20: Write down a lambda term whose normal form is not a value.
-- (\x. xx)(\x. xx)

-- Question 21: Recall that omega is the simplest term that loops
-- forever, for some definition of simplest. Come up with a term that
-- does not have a normal form, *and* doesn't "reduce", in the sense
-- that it evaluates to a larger and larger expressions as you apply
-- reduction rules.
-- (\x. xxx)(\x. xxx)

-- Part 2: Imperative Languages and Big-Step Operational Semantics

-- Syntax for arithmetic expressions:
data AExp =
       Variable String
     | Const Int
     | Plus AExp AExp
     | Minus AExp AExp
     
-- Syntax for boolean expressions:
data BExp =
     BTrue | BFalse |
     BNot BExp | BAnd BExp BExp |
     BEq AExp AExp | BLt AExp AExp
     
-- Imperative languages like C and Java often include a break or
-- similar statement for interrupting the execution of loops. In this
-- exercise we consider how to add break to Imp. First, we need to
-- enrich the language of commands with an additional case.

data ImpExp =
     Assign String AExp | Skip |
     Seq ImpExp ImpExp | IfThenElse BExp ImpExp ImpExp |
     While BExp ImpExp | Break
    

-- Next, we need to define the behavior of [break]. Informally, whenever
-- [break] is executed in a sequence of commands, it stops the execution
-- of that sequence and signals that the innermost enclosing loop
-- should terminate. (If there aren't any enclosing loops, then the
-- whole program simply terminates.) The final state should be the
-- same as the one in which the [break] statement was executed.  One
-- important point is what to do when there are multiple loops
-- enclosing a given [break]. In those cases, [break] should only
-- terminate the innermost loop. Thus, after executing the
-- following...

--        x := 0;
--        y := 1;
--        while 0 ≠ Y do {
--          while true do {
--            break
--          }
--          x := 1;
--          y := y - 1
--       }
-- ... the value of [x] should be 1, and not 0.

-- One way of expressing this behavior is to add another parameter to
-- the evaluation relation that specifies whether evaluation of a
-- command executes a [break] statement:

data Result =
       ContinueSignal
     | BreakSignal
     
-- Intuitively, σ, exp ⇓ σ', res means that, if exp is started in state
-- σ, then it terminates in state σ' and either signals that the
-- innermost surrounding loop (or the whole program) should exit
-- immediately (res is BreakSignal) or that execution should continue normally
-- (res is ContinueSignal).

-- The definition of the "σ, exp ⇓ σ', res" relation is very similar
-- to one for the big-step semantics for Imp we saw in class ("σ, exp ⇓ σ'")
-- — we just need to handle the termination signals
-- appropriately:

-- 1.  If the command is [skip], then the state doesn't change and
-- execution of any enclosing loop can continue normally.

-- 2.  If the command is [break], the state stays unchanged but we
-- signal a BreakSignal.

-- 3.  If the command is an assignment, then we update the binding for
-- that variable in the state accordingly and signal that execution
-- can continue normally.

-- 4.  If the command is of the form [if b then c1 else c2], then the
-- state is updated as in the original semantics of Imp, except that
-- we also propagate the signal from the execution of whichever branch
-- was taken.

-- 5.  If the command is a sequence [c1; c2], we first execute c1. If
-- this yields a BreakSignal, we skip the execution of c2 and propagate the
-- BreakSignal signal to the surrounding context; the resulting state is the
-- same as the one obtained by executing c1 alone. Otherwise, we
-- execute c2 on the state obtained after executing c1, and propagate
-- the signal generated there.

--  6.  Finally, for a loop of the form [while b do { c }], the
--  semantics is almost the same as before. The only difference is
--  that, when b evaluates to true, we execute c and check the signal
--  that it raises. If that signal is ContinueSignal, then the execution
--  proceeds as in the original semantics. Otherwise, we stop the
--  execution of the loop, and the resulting state is the same as the
--  one resulting from the execution of the current iteration. In
--  either case, since [break] only terminates the innermost loop,
--  [while] signals ContinueSignal.

-- Question 22: Based on the above description, write down the rules
-- for the big-step semantics of Imp with break statements. (This is
-- the equivalent of 10 'normal' questions) Here are Imp's old big-step
-- semantics for reference:
--
--    ------------ (E_Skip)
--    skip / σ ⇓ σ
--
--    σ, a1 ⇓ n
--    ------------  	(E_Ass)
--    σ, x := a1 ⇓ σ [x --> n]
--
--    σ, c1  ⇓ σ'         σ', c2  ⇓ σ''
--    -------------------------------- (E_Seq)
--    σ, c1; c2 ⇓ σ''
--
--    σ, b1 ⇓ true    σ, c1 ⇓ σ'
--    --------------------------------- (E_IfTrue)
--    σ, if b1 then c1 else c2 ⇓ σ'
--
--    σ, b1 ⇓ false        c2 / σ ⇓ σ'
--    --------------------------------- (E_IfFalse)
--   σ, if b1 then c1 else c2  ⇓ σ'

--   σ, b ⇓ false
--   -------------------  	(E_WhileFalse)
--    σ, while b do { c }⇓ σ

--   σ, b ⇓ true        σ, c ⇓ σ'     σ', while b do {c} ⇓ σ''
--    -------------------------------------------------------- (E_WhileTrue)
--    σ, while b do { c } ⇓ σ''


-- YOUR RULES GO HERE!
-- ø represents a signal

--    ------------ (E_Skip)
--    skip / σ, ø ⇓ σ, ø

--    ------------ (E_Break)
--    break / σ, ø ⇓ σ, (ø = BREAK)

--    σ,ø, a1 ⇓ n
--    ------------  	(E_Ass)
--    σ,ø, x := a1 ⇓ σ,ø [x --> n]

--    σ,ø, c1  ⇓ σ',ø   {ø' == CONTINUE} σ', c2  ⇓ σ'', ø''
--    ---------------------------------------------- (E_Seq_Continue)
--    σ,ø, c1; c2 ⇓ σ'', ø''

--    σ,ø, c1  ⇓ σ',ø'   {ø' == BREAK}
--    ---------------------------------------------- (E_Seq_Break)
--    σ,ø, c1; c2 ⇓ σ', ø'

--    σ, b1 ⇓ true    σ, c1 ⇓ σ'
--    --------------------------------- (E_IfTrue) (propogate signal from branch)
--    σ, if b1 then c1 else c2 ⇓ σ'
--
--    σ,ø, b1 ⇓ false        c2 / σ ⇓ σ',ø'
--    --------------------------------- (E_IfFalse) (propogate signal from branch)
--   σ,ø, if b1 then c1 else c2  ⇓ σ',ø'

--   σ,ø, b ⇓ false
--   -------------------  	(E_WhileFalse)
--    σ,ø, while b do { c }⇓ σ,ø

--   σ,ø, b ⇓ true     σ,ø, c ⇓ σ',ø'    σ',{ø'==CONTINUE}, while b do {c} ⇓ σ'',ø''
--    ------------------------------------------------------------------------------(E_WhileTrue_CONTINUE) 
--    σ,ø, while b do { c } ⇓ σ'',ø''

--   σ,ø, b ⇓ true     σ,ø, c ⇓ σ',ø'  {ø'==BREAK}
--    --------------------------------------------(E_WhileTrue_BREAK)
--    σ,ø, while b do { c } ⇓ σ',ø'

--  Having formulated the semantics of Imp+Break, it's time to write
--  an interpreter! We begin with the definitions from Imp.hs we saw
--  in class.

-- Stores are maps from variable names (Strings) to integers
-- The empty store throws an exception :p
emptyStore :: Store
emptyStore _ = error "Oh No!"

-- Updating a store takes an old store, a key, and a value, and
-- returns a new store (function)
updateStore :: Store -> String -> Int -> Store
updateStore s id v = \id' -> if id' == id then v else s id'

type Store = String -> Int

updateStoreM :: String -> Int -> State Store ()
updateStoreM id v = state $ \s -> ((), updateStore s id v)

getStoreM :: String -> State Store Int
getStoreM id = state $ \s -> (s id, s)

-- Interpreter for arithmetic expressions
evalAM :: AExp -> State Store Int
evalAM (Variable v) = getStoreM v
evalAM (Const i) = return i
evalAM (Plus a1 a2) = do
       m <- evalAM a1
       n <- evalAM a2
       return (m + n)
evalAM (Minus a1 a2) = do
       m <- evalAM a1
       n <- evalAM a2
       return (m - m)

-- Interpreter for boolean expressions:
evalBM :: BExp -> State Store Bool
evalBM BTrue = return True
evalBM BFalse = return False
evalBM (BNot a1) = do
        v <- evalBM a1
        return $ not v
evalBM (BAnd a1 a2) = do
        b1 <- evalBM a1
        b2 <- evalBM a2
        return (b1 && b2)
evalBM (BEq a1 a2) = do
        m <- evalAM a1
        n <- evalAM a2
        return (m == n)
evalBM (BLt a1 a2) = do
        m <- evalAM a1
        n <- evalAM a2
        return (m < n)

-- Question 23: Using the rules above, write an interpreter for Imp
-- with break statements. Use the State monad, as we discussed in
-- class, to simplify your interpreter (This is again the equivalent
-- of 10 'normal' questions):

evalImpM :: ImpExp -> State Store Result
evalImpM (Assign s am)              = do 
                                        rhs <- evalAM am
                                        s <- updateStoreM s rhs
                                        return ContinueSignal
evalImpM (Skip)                     = return ContinueSignal
evalImpM (Break)                    = return BreakSignal
evalImpM (Seq imp1 imp2)            = do
                                        res1 <- evalImpM imp1
                                        case res1 of
                                          (BreakSignal)     -> return res1
                                          (ContinueSignal)  -> evalImpM imp2
evalImpM (IfThenElse be imp1 imp2)  = do
                                        cond <- evalBM be
                                        if cond
                                          then evalImpM imp1
                                          else evalImpM imp2
evalImp (While be imp)              = do
                                        cond <- evalBM be
                                        if cond
                                          then evalImpM $ Seq imp (While be imp)
                                          else return ContinueSignal

main = print $  "Does anyone read this line? \n yes."
