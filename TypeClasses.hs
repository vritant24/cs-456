-- Show how to use :info TypeClasses

data Aexp =
       Variable String
     | Const Int
     | Plus Aexp Aexp
     | Minus Aexp Aexp

printAexp :: Aexp -> String
printAexp (Variable v) = v
printAexp (Const i) = show i
printAexp (Plus a1 a2) = printAexp a1 ++ "+" ++ printAexp a2
printAexp (Minus a1 a2) = printAexp a1 ++ "-" ++ printAexp a2

instance Show Aexp where show = printAexp


instance Enum Aexp where
         toEnum i = (Const i)
         fromEnum (Const i) = i
         fromEnum (Variable _) = 0
         fromEnum (Plus a1 a2) = fromEnum a1 + fromEnum a2
         fromEnum (Minus a1 a2) = (fromEnum a1) - (fromEnum a2)
-- fromEnum (Plus (fromEnum 1) (Const 2)) = 3
-- fromEnum (Plus (toEnum 1) (Const 4)) = 5

data PolyAexp v i =
       VariableP v
     | ConstP i
     | PlusP (PolyAexp v i) (PolyAexp v i)
     | MinusP (PolyAexp v i) (PolyAexp v i)

printPolyAexp :: (Show v, Show i) => PolyAexp v i -> String
printPolyAexp (VariableP v) = show v
printPolyAexp (ConstP i) = show i
printPolyAexp (PlusP a1 a2) = printPolyAexp a1 ++ "+" ++ printPolyAexp a2
printPolyAexp (MinusP a1 a2) = printPolyAexp a1 ++ "-" ++ printPolyAexp a2

instance (Show v, Show i) => Show (PolyAexp v i) where show = printPolyAexp

-- (PlusP (ConstP "a") (VariableP "q")) --> "a"+"q"
-- (Plus (Const 1) (Const 2)) --> 1+2

-- Definining your own typeclasses:
class YesNo a where
      yesno :: a -> Bool

instance YesNo Bool where
         yesno b = b

instance YesNo [a]  where
         yesno [ ] = False
         yesno _ = True

instance YesNo Aexp where
         yesno (Variable _) = False
         yesno (Const _) = False
         yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf i t e = if (yesno i) then t else e

main = print $ (Const 1)
