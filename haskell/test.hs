isTrue = True
isFalse = False

-- `:t` always gives type
-- eg - `:t isTrue` fives flase

double x = x + x
doubleSmall x = if x < 100 then double x else x

navi = "Hey, listen!"
k = 'k'

l = [1, 2, 3]
l' = [1..100]
l'' = [2, 4..20]
l''' = [1..]

-- List comprehension (like set builder notation)
evens = [x * 2 | x <- [1..10]]
evens' = [x | x <- [1..20], even x]
length' l = sum [1 | _ <- l]

--tuple (toople), a product type

p = (1, 2)
p' = (1, 2, "")

-- pattern matching like scala
fst' (a, _, _) = a
snd' (_, b, _) = b
trd' (_, _, c) = c

-- declaring a sum type (like enum)
data Bool' = True' | False' 

-- functions
fac :: Integer -> Integer 
fac x = if x == 0 then 1 else x * fac (x-1)

fac' :: Integer -> Integer
fac' 0 = 1
fac' x = x * fac'(x - 1)

showMe :: Integer -> String
showMe 1 = "one"
showMe _ = "unknown"

-- currying
makeAdder :: Integer -> Integer -> Integer
makeAdder x y = x + y -- can be partially applied

--zip func

zip' :: [Integer] -> [Integer] -> [(Integer, Integer)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []