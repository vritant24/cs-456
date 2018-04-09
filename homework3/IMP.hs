import Control.Monad.State
import Control.Monad.Identity

-- Syntax for arithmetic expressions:
data AExp =
       Variable String
     | Const Int
     | Plus AExp AExp
     | Times AExp AExp

instance Show AExp where
         show (Variable v) =  v -- :: AExp -> string
         show (Const i) = show i
         show (Plus a1 a2) = show a1 ++ " + " ++ show a2
         show (Times a1 a2) = show a1 ++ " * " ++ show a2

-- An interpreter for pure arithmetic expressions:
evalA :: AExp -> Int
evalA (Const i) = i
evalA (Plus a1 a2) = evalA a1 + evalA a2
evalA (Times a1 a2) = evalA a1 * evalA a2

-- An encoding of the literal 1
exp1 = Const 1
-- An encoding of x * 2
exp3 = Times (Variable "x") (Const 2)

-- Syntax for boolean expressions:
data BExp =
     BTrue | BFalse |
     BNot BExp | BAnd BExp BExp |
     BEq AExp AExp | BLt AExp AExp

instance Show BExp where
         show BTrue =  "true" -- :: BExp -> string
         show BFalse = "false"
         show (BNot a1) = "¬" ++ show a1
         show (BAnd a1 a2) = show a1 ++ " && " ++ show a2
         show (BEq a1 a2) = show a1 ++ " == " ++ show a2
         show (BLt a1 a2) = show a1 ++ " < " ++ show a2

-- An interpreter for pure boolean expressions:
evalB :: BExp -> Bool
evalB BTrue =  True
evalB BFalse = False
evalB (BNot a1) = not $ evalB a1
evalB (BAnd a1 a2) = evalB a1 && evalB a2
evalB (BEq a1 a2) = evalA a1 == evalA a2
evalB (BLt a1 a2) = evalA a1 < evalA a2

-- An encoding of (true && (1 == 3))
exp2 = (BAnd BTrue (BEq (Const 1) (Const 3)))

-- Stores are maps from variable names (Strings) to integers
type Store = String -> Int

-- The empty store throws an exception :p
emptyStore :: Store
emptyStore _ = error "Oh No!"

-- Updating a store takes an old store, a key, and a value, and returns an new store (function)
updateStore :: Store -> String -> Int -> Store
updateStore s id v = \id' -> if id' == id then v else s id'

-- The evaluator for arithmetic expressions with variables takes an additional store argument to lookup variables values
evalAS :: AExp -> Store -> Int
evalAS (Variable v) s = s v
evalAS (Const i) s = i
evalAS (Plus a1 a2) s = evalAS a1 s + evalAS a2 s
evalAS (Times a1 a2) s = evalAS a1 s * evalAS a2 s

-- Ditto for the evaluator for boolean expressions
evalBS :: BExp -> Store -> Bool
evalBS BTrue s =  True
evalBS BFalse s = False
evalBS (BNot a1) s = not $ evalBS a1 s
evalBS (BAnd a1 a2) s = evalBS a1 s && evalBS a2 s
evalBS (BEq a1 a2) s = evalAS a1 s == evalAS a2 s
evalBS (BLt a1 a2) s = evalAS a1 s < evalAS a2 s

-- Syntax for imp statements
data ImpExp =
     Assign String AExp | Skip |
     Seq ImpExp ImpExp | IfThenElse BExp ImpExp ImpExp |
     While BExp ImpExp

instance Show ImpExp where
         show (Assign x v) =  x ++ " := " ++ show v
         show Skip = "skip"
         show (Seq s1 s2) = show s1 ++ "; " ++ show s2
         show (IfThenElse c t e) = "If (" ++ show c ++ ") Then " ++ show t ++ " Else " ++ show e
         show (While c s) = "While (" ++ show c ++ ")  do { " ++ show s ++ "}"

-- An encoding of while (x < 10) do {x := x + 1}
exp4 = While (BLt (Variable "x") (Const 10)) (Assign "x" (Plus (Variable "x") (Const 1)))

-- An evaluation function for imp takes a store and returns an updated store.
evalImp :: ImpExp -> Store -> Store
evalImp (Assign x v) s =  updateStore s x (evalAS v s)
evalImp Skip s = s
evalImp (Seq s1 s2) s = let s' = evalImp s1 s in evalImp s2 s'
evalImp (IfThenElse c t e) s = if evalBS c s then evalImp t s else evalImp e s
evalImp (While c b) s = if evalBS c s then let s' = evalImp b s in evalImp (While c b) s' else s

-- A Detour into Monads From the textbook

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

-- landLeft 2 (0,0)
-- landRight 1 (1,2)
-- landRight (-1) (1,2)

-- landLeft 2 (landRight 1 (landLeft 1 (0,0)))

x -: f = f x

-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
         | abs ((left + n) - right) < 4 = Just (left + n, right)
         | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
          | abs (left - (right + n)) < 4 = Just (left, right + n)
          | otherwise                    = Nothing

-- landRight 1 (0,0) >>= landLeft 2
-- Nothing >>= landLeft 2
-- return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

-- Syntax for arithmetic expressions, now with division:
data ADExp =
       VariableD String
     | ConstD Float
     | PlusD ADExp ADExp
     | TimesD ADExp ADExp
     | DivideD ADExp ADExp

instance Show ADExp where
         show (VariableD s) = s
         show (ConstD i) = show i
         show (PlusD a1 a2) = show a1 ++ " + " ++ show a2
         show (TimesD a1 a2) = show a1 ++ " * " ++ show a2
         show (DivideD a1 a2) = show a1 ++ " / " ++ show a2

-- Let's define a "throw exception" operation for the interpreter
throwError :: Maybe a
throwError = Nothing

-- The evaluation function for division can now throw an exception using the maybe monad:
evalAD :: ADExp -> Maybe Float
evalAD (ConstD i) = return i
evalAD (PlusD a1 a2) = do
       m <- evalAD a1
       n <- evalAD a2
       return (m + n)
evalAD (TimesD a1 a2) = do
       m <- evalAD a1
       n <- evalAD a2
       return (m * n)
evalAD (DivideD a1 a2) = do
       m <- evalAD a1
       n <- evalAD a2
       if (n == 0) then
        throwError
       else
        return (m / n)

-- evalAD (DivideD (PlusD (ConstD 1) (ConstD 2)) (TimesD (ConstD 2) (ConstD 3)))

-- There is also a monad for representing stateful programs in Haskell
-- (called State s), appropriately enough. Let's see how the previous
-- interpreter for Imp can be written in the monadic style.

-- We begin by writing functions for updating and looking up values in
-- the store that is threaded through our stateful interpreter:
updateStoreM :: String -> Int -> State Store ()
updateStoreM id v = state $ \s -> ((), updateStore s id v)

getStoreM :: String -> State Store Int
getStoreM id = state $ \s -> (s id, s)

-- We'll use getStoreM to look up the value of a variable in our
-- monadic evaluation function. Note that how similar this definition
-- is to evalAD, even though they have quite different effects!
evalAM :: AExp -> State Store Int
evalAM (Variable v) = getStoreM v
evalAM (Const i) = return i
evalAM (Plus a1 a2) = do
       m <- evalAM a1
       n <- evalAM a2
       return (m + n)
evalAM (Times a1 a2) = do
       m <- evalAM a1
       n <- evalAM a2
       return (m * m)

-- Evaluation for boolean expressions:
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

-- Note that we use the unit type () for the type of values wrapped by
-- the State monad in the evaluation function for Imp statements. The
-- unit type has a single constructor with no arguments, which is
-- somewhat confusingly also named ().

evalImpM :: ImpExp -> State Store ()
evalImpM (Assign x a) = do
         v <- evalAM a
         updateStoreM x v
evalImpM Skip = return () -- This is the constructor for the unit type
evalImpM (Seq s1 s2) = do
         x <- evalImpM s1
         evalImpM s2
evalImpM (IfThenElse c t e) = do
         b <- evalBM c
         if b then evalImpM t else evalImpM e
evalImpM (While c b) = do
         b' <- evalBM c
         if b' then do
                  x <- evalImpM b
                  evalImpM (While c b)
              else return ()

ex7 = let (_, s') = runState (evalImpM exp4) (updateStore emptyStore "x" 0) in s' "x"

-- Once we've added division, our variables can map to floats, so we
-- need to update the type of values in Stores. Additionally, since we
-- can now represent exceptions using the maybe monad, we can
-- explicitly represent "variable not found" as Nothing, and make
-- stores return a Maybe value.

type FStore = String -> Maybe Float

-- The empty store returns Nothing
emptyFStore x = Nothing

-- An updated store returns a Just value for a found key
updateFStore :: FStore -> String -> Float -> FStore
updateFStore s id v = \id' -> if id' == id then Just v else (s id')

-- StateT layers the state monad on top of another one. We can apply
-- it to Maybe to build a monad that represents both state and
-- exceptions.

type MaybeState a = StateT FStore Maybe a

-- We need to update our update store, lookup id, and throw exception
-- functions for this monad. We'll throw in a catch operation for good
-- measure.
updateFStoreM :: String -> Float -> MaybeState ()
updateFStoreM id v = StateT $ \s -> Just ((), updateFStore s id v)

getFStoreM :: String -> MaybeState Float
getFStoreM id = StateT $ \s -> case s id of
                               Nothing -> Nothing
                               Just v -> Just (v, s)

throwErrorM :: MaybeState a
throwErrorM = StateT $ \s -> Nothing

catchErrorM :: MaybeState a -> MaybeState a -> MaybeState a
catchErrorM e1 e2 = StateT $ \s ->
                             case runStateT e1 s of
                              Nothing -> runStateT e2 s
                              Just v -> Just v

-- Again, note how similar this evaluation function is to the previous
-- two monadic evaluation functions.
evalADM :: ADExp -> MaybeState Float
evalADM (VariableD v) = getFStoreM v
evalADM (ConstD i) = return i
evalADM (PlusD a1 a2) = do
       m <- evalADM a1
       n <- evalADM a2
       return (m + n)
evalADM (TimesD a1 a2) = do
       m <- evalADM a1
       n <- evalADM a2
       return (m * m)
evalADM (DivideD a1 a2) = do
        m <- evalADM a1
        n <- evalADM a2
        if (n == 0) then
         throwErrorM
        else
         return (m / n)

-- We need to update the datatype for boolean expressions to refer to
-- the new arithmetic expression syntax.
data BDExp =
     BTrueD | BFalseD |
     BNotD BDExp | BAndD BDExp BDExp |
     BEqD ADExp ADExp | BLtD ADExp ADExp

-- Ditto for the evaluation function.
evalBDM :: BDExp -> MaybeState Bool
evalBDM BTrueD = return True
evalBDM BFalseD = return False
evalBDM (BNotD a1) = do
       v <- evalBDM a1
       return $ not v
evalBDM (BAndD a1 a2) = do
       b1 <- evalBDM a1
       b2 <- evalBDM a2
       return (b1 && b2)
evalBDM (BEqD a1 a2) = do
       m <- evalADM a1
       n <- evalADM a2
       return (m == n)
evalBDM (BLtD a1 a2) = do
       m <- evalADM a1
       n <- evalADM a2
       return (m < n)

-- We can add try statements to Imp using our catchM function:
data ImpDExp =
     AssignD String ADExp | SkipD |
     SeqD ImpDExp ImpDExp | IfThenElseD BDExp ImpDExp ImpDExp |
     WhileD BDExp ImpDExp | TryD ImpDExp ImpDExp

evalImpDM :: ImpDExp -> MaybeState ()
evalImpDM (AssignD x a) = do
         v <- evalADM a
         updateFStoreM x v
evalImpDM SkipD = return ()
evalImpDM (SeqD s1 s2) = do
         x <- evalImpDM s1
         evalImpDM s2
evalImpDM (IfThenElseD c t e) = do
         b <- evalBDM c
         if b then evalImpDM t else evalImpDM e
evalImpDM (WhileD c b) = do
         b' <- evalBDM c
         if b' then do
                  x <- evalImpDM b
                  evalImpDM (WhileD c b)
              else return ()
evalImpDM (TryD s1 s2) =
          catchErrorM (evalImpDM s1) (evalImpDM s2)

exp5 = WhileD (BLtD (VariableD "x") (ConstD 10)) (AssignD "x" (PlusD (VariableD "x") (ConstD 1)))
exp6 = WhileD (BLtD (VariableD "x") (ConstD 10)) (AssignD "x" (DivideD (VariableD "x") (VariableD "y")))

-- let Just (x, _) = runStateT (evalADM (DivideD (PlusD (ConstD 1) (ConstD 2)) (TimesD (ConstD 2) (ConstD 3)))) emptyFStore in x
-- let Just (x, _) = runStateT (evalADM (DivideD (VariableD "x") (TimesD (ConstD 2) (ConstD 3)))) (updateFStore emptyFStore "x" 1) in x
-- let Just (x, s) = runStateT (evalImpDM exp5) (updateFStore emptyFStore "x" 1) in s "x"

main = print $ (evalImp exp4 (updateStore emptyStore "x" 0)) "x"
