-- CS 456 Homework 1
-- Due date: 02/02/2018 by 9:00PM

-- Name: Vritant Bhardwaj   
-- Email: bhardwav@purdue.edu

-- ASSIGNMENT INSTRUCTIONS:
-- ========================================================================
-- All questions have to be completed. Before submitting, make sure
-- that running 'ghc hw1.hs' in the shell completes without errors.
-- If your homework does not compile, it will not be graded! If an
-- incomplete answer to a question is causing ghc to fail, comment it
-- out.

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- Submit all of your files via turnin using the instructions here (windows
-- users will perform a similar incantation):
-- https://www.cs.purdue.edu/homes/cs177/turnin-from-home-macos.html
-- Use cs456 for the class name and hw1 for the project name:
-- ssh <your username>@lore.cs.purdue.edu /usr/local/bin/turnin -c cs456 -p hw1 <your files>


-- Part 1: Haskell Basics
-- For this portion of the homework, you'll be writing haskell
-- functions to help folks train for a triathlon. (Don't worry, you
-- won't be asked to do any training yourself. )

-- A RunRecord is a haskell value of the type (String, Float, Int),
-- where the first entry is the run's location, the second entry is
-- the total run distance in meters, and the third entry is the total
-- run time in seconds.
type RunRecord = (String, Float, Float)

--Question 1: Write a function [pace] that calculates the average pace of a run in m/s.
pace :: RunRecord -> Float
pace (_, dist, time) = dist / time

-- Question 2: Write a function [comparePace] which takes two
-- RunRecords and returns the RunRecord with the better pace

comparePace :: RunRecord -> RunRecord -> RunRecord
comparePace r1 r2 = if ((pace r1) > (pace r2)) then r2 else r1

-- Question 3: Write a function [bestPace] that takes a list of
-- RunRecords and a location (String) and returns the best pace for
-- that location. You can assume that the pace is never less than 0.
bestPace :: [RunRecord] -> String -> Float
bestPace [] _ = 0
bestPace recs loc = pace (
        foldl ( \acc x@(l, d, t) -> 
                if (l == loc) 
                    then comparePace acc x 
                    else acc
        ) (loc, 0, 0) recs
    )

-- Question 4: Write a function that takes a list of RunRecords and a
-- list of locations and returns a list of pairs of (location, Float)
-- holding the location and best pace for each location in the list
bestPaces :: [RunRecord] -> [String] -> [(String, Float)]
bestPaces _ [] = []
bestPaces [] locs = map (\loc -> (loc, 0)) locs
bestPaces recs locs = map (\loc -> (loc, bestPace recs loc)) locs 

--Question 5: I would like to know how much more practice runs I need
--before I hit my target pace for an upcoming triathlon (it's probably
--a lot). Write a function that takes a list of my previous runs in
--chronological order, and a target pace, and calculates that number
--of runs it will take. (hint: use the list of previous runs to
--calculate average improvement on a per-race basis.)
-- howManyMore :: [RunRecord] -> Float -> Int
-- howManyMore list p = round ((p - prev) `div` (tot `div` (length list)))
--                     where (prev, tot) = foldl ( \(prev, total) x-> 
--                                             ((pace x), (total + (prev - (pace x))))
--                                         ) (0.0, 0.0) list 

-- Part 2: Ill-Typed Expressions
-- Explain why each of the following programs fail to type check by in
-- the strings explain[6-8], and change the code in some small way so
-- that they do. problem[6-8] should be uncommented in the file you
-- submit. Note: Don't change the type of the expression!

explain6 :: String
explain6 = "the LHS is a list of chars and the RHS is a char. They are different types, so they don't pass the typecheck."

problem6 :: Bool
-- problem6 = "a" == 'a'
problem6 = 'a' == 'a'

explain7 :: String
explain7 = "The function was being called with a tuple of (RunRecord, RunRecord) instead of two separate arguments"

problem7 :: RunRecord
-- problem7 = comparePace(("Chicago", 1000, 360), ("Austin", 2000, 240))
problem7 = comparePace ("Chicago", 1000, 360) ("Austin", 2000, 240)

explain8 :: String
explain8 = "p1 was instantiated as a tuple of (n, n), which was then inserted into the tuple as ((n,n), n). This made the return type ((Int, Int), Int), when it should be (Int, Int, Int)."

problem8 :: (Int, Int, Int)
-- problem8 = tripleIt 1
--   where tripleIt n = let p1 = (n, n) in (p1, n)
problem8 = tripleIt 1
  where tripleIt n = let p1 = (n, n, n) in (p1)


-- Part 3: Inductive Data Structures and recursion
-- Here is the [IntTree] data type from class; in the next three
-- questions, show how to encode the given trees using this data type.

data IntTree =
     Node Int IntTree IntTree
     | Leaf


-- Question 9.
--         2
--        / \
--       /   \
--      /     \
--     1       3
--            / \
--           9   1
question9 :: IntTree
question9 = Node 2 
                (Node 1 Leaf Leaf) 
                (Node 3 
                    (Node 9 Leaf Leaf) 
                    (Node 1 Leaf Leaf)
                )

-- Question 10.
--      10
--      / \
--     5   8
--        /
--       2
question10 :: IntTree
question10 = Node 10 
                (Node 5 Leaf Leaf) 
                (Node 8 (Node 2 Leaf Leaf) Leaf)

-- Question 11.
--         2
--        / \
--       /   \
--      /     \
--     1       3
--    / \     / \
--   0   7   9   1
--  /   / \     / \
-- 2   1   0   8   8
--        /
--       7
question11 :: IntTree
question11 = Node 2 
    (Node 1 
        (Node 0 (Node 2 Leaf Leaf) Leaf) 
        (Node 7 
            (Node 1 Leaf Leaf) 
            (Node 0 (Node 7 Leaf Leaf) Leaf)
        )
    )
    (Node 3 
        (Node 9 Leaf Leaf) 
        (Node 1 
            (Node 8 Leaf Leaf) 
            (Node 8 Leaf Leaf)
        )
    )

-- Question 12. Write a membership function that checks if an integer
-- is in an IntTee (note that the tree need not be balanced).

membership :: IntTree -> Int -> Bool
membership (Leaf) _ = False
membership (Node n l r) i = if (n == i) 
                            then True 
                            else (membership l i) || (membership r i) 

-- Part 4. Polymorphism and higher-order functions.

-- The Maybe type (also called option) is commonly used to represent
-- errors or exceptional cases in functions without throwing an
-- runtime error. The type has two constructors:
-- data Maybe a = Just a | Nothing
-- Think of the Just constructor as representing a normal
-- value of type a, while None as representing "undefined case". As an
-- example, consider this division function:
division :: Float -> Float -> Maybe Float
division n m = if m == 0 then Nothing else Just (n / m)

-- Question 13. Write the getNth function, which returns the nth
-- element of a list or None if the index would go beyond the end of a
-- list.
-- getNth [2, 4, 6, 8] 3 = Just 8
-- getNth [2, 4, 6, 8] 4 = Nothing

getNth :: [a] -> Int -> Maybe a
getNth [] _ = Nothing
getNth arr n =  if (n >= length arr || n < 0) 
                then Nothing
                else Just (arr !! n)

-- A function calling division or getNth can thus "catch" errors by
-- checking the value of the result using a case expression. This
-- pattern is so prevalent that we may want to write a specialized
-- catch function to capture it.

-- Question 14. Write such a catch function. Here are some example outputs:
-- catch Nothing (\x. x + x) 0 = 0
-- catch (Just 3) (\x. x + x) 0 = 6
-- catch (Just 15) (\x. x + x) 0 = 30

catch :: Maybe a -> (a -> b) -> b -> b
catch Nothing _ err = err
catch (Just x) f _ = f x

-- The Maybe type is also quite useful for representing partial
-- functions (like division).

-- Question 15.
-- Write a specialized polymorphic map function that takes such a
-- partial function and maps it over the elements of a list, dropping
-- elements on which the function is undefined.

-- Examples:
-- mapPartial (division 2) [6, 5, 8, 0, 10] = [3, 2.5, 4, 5]
-- mapPartial (getNth ["a", "b", "c", "d", "e", "f"]) [0, 10, 3, 0] = ["a", "d", "a"]

mapPartial :: (a -> Maybe b) -> [a] -> [b]
mapPartial _ [] = []
mapPartial f l = foldl  (\acc x -> 
                            acc ++ (
                                catch (f x) (\x1 -> [x1]) []
                            )
                        ) 
                        [] l

-- Question 16.
-- Recall that a function applied to a single tuple of arguments is equivalent to a function
-- that takes an argument and returns a function that takes the second argument. Write
-- curry and uncurry functions that performs this conversion:

curry_ :: ((a, b) -> c) -> a -> b -> c
curry_ f = \a b -> f (a, b)

uncurry_ :: (a -> b -> c) -> (a, b) -> c
uncurry_ f = \(a, b) -> f a b

-- Part 5
-- In this portion of the homework, you will implement a simple
-- pattern matching language, akin to how one would implement the case
-- expressions in Haskell:

-- We begin with inductive datatypes whose values represent the
-- patterns and the corresponding values they match against.

data Pattern =
     WildCardP
     | VariableP String
     | UnitP
     | ConstP Int
     | TupleP [Pattern]
     | ConstructorP String Pattern

data Value =
     ConstV Int
     | UnitV
     | TupleV [Value]
     | ConstructorV String Value

-- Given v :: Value and p :: Pattern, either p matches v or not. If it does, the match produces
-- a list of (String, Value) pairs showing how values in v are 'bound' to pattern variables in p;
-- order in the list does not matter. The rules for matching should be unsurprising:
-- • Wildcard{ matches everything and produces the empty list of bindings.
-- • VariableP s matches any value v and produces the one-element list holding (s,v).
-- • UnitP matches only Unit and produces the empty list of bindings.
-- • ConstP 17 matches only ConstV 17 and produces the empty list of bindings (and similarly for other integers).
-- • TupleP ps matches a value of the form TupleV vs if ps and vs have the same length and for all i, the ith
--   element of ps matches the ith element of vs. The list of bindings produced is all the lists from the
--   nested pattern matches appended together.
-- • ConstructorP s1 p matches ConstructorV s2 v if s1 and s2 are the same String (you can compare them with ==)
--   and p matches v. The list of bindings produced is the list from the nested pattern match. We call the strings
--   s1 and s2 the 'constructor name'.
-- • Nothing else matches.

-- Question 17. g is an intentially opaquely-named function that does
-- something with patterns. In String 17, give g's type
-- signature and describe in a few sentences what arguments g takes
-- and what g computes.

g f1 f2 WildCardP = f1
g f1 f2 (VariableP var) = f2 var
g f1 f2 (ConstructorP name pat) = g f1 f2 pat
g f1 f2 (TupleP pats) = foldl (\ n p -> g f1 f2 p + n) 0 pats
g _ _ _ = 0

explain17 :: String
explain17 = "g :: Int -> (String -> Int) -> Pattern -> Int"

-- Question 18. Use g to define a function countWildCards that takes a
-- pattern and returns how many Wildcard patterns it contains.
countWildCards :: Pattern -> Int
countWildCards p = g 1 (\x -> 0) p

-- Question 19. Use g to define a function
-- countWildCardsAndVariableLengths that takes a pattern and returns
-- the number of Wildcard patterns it contains plus the sum of the
-- String lengths of all the variables in the variable patterns it
-- contains.  We care only about variable names; the
-- constructor names are not relevant.
countWildCardsAndVariableLengths :: Pattern -> Int
countWildCardsAndVariableLengths pat = g 1 (\x -> length x) pat

-- Question 20. Use g to define a function countSomeVar that takes a
-- String and a pattern and returns the number of times the string
-- appears as a variable in the pattern. We care only about variable
-- names; the constructor names are not relevant.
countSomeVar :: String -> Pattern -> Int
countSomeVar s pat = g 0 (\n -> if (n == s) then length n else 0) pat

-- Question 21. Write a function checkPat that takes a pattern and
-- returns true if and only if all the variables appearing in the
-- pattern are distinct from each other (i.e., use different
-- strings). The constructor names are not relevant. Hints: The sample
-- solution uses two helper functions. The first takes a pattern and
-- returns a list of all the strings it uses for variables. Using
-- foldl with a function that uses append is useful in one case. The
-- second takes a list of strings and decides if it has
-- repeats. Sample solution is ~15 lines.

checkPat :: Pattern -> Bool
checkPat p = foldl (\acc s ->
                        if ((getCount s l) > 1) then False else acc
                    ) True l
            where l = getVars p

getVars :: Pattern -> [String]
getVars (WildCardP) = []
getVars (VariableP s) = [s]
getVars (UnitP) = []
getVars (TupleP ps) = foldl (\acc p -> acc ++ (getVars p)) [] ps
getVars (ConstructorP _ p) = getVars p

getCount :: String -> [String] -> Int
getCount _ [] = 0
getCount s list = foldl (\acc x -> if(x == s) then acc + 1 else acc) 0 list


-- Question 22. Write a function match that takes a Value and a
-- Pattern and returns a Maybe [(String, Value)], returning Nothing if
-- the pattern does not match and Just lst where lst is the list of
-- bindings if it does. Note that if the value matches but the pattern
-- has no patterns of the form Variable s, then the result is Just
-- []. Hints: Sample solution has one case expression with 7
-- branches. Our sample solution is ~15 lines. Remember to look above
-- for the rules for what patterns match what values, and what
-- bindings they produce.
match :: Value -> Pattern -> Maybe [(String, Value)]
match v p = case p of   
                (WildCardP) -> Just []
                (VariableP s) -> Just [(s, v)]
                (UnitP) -> var
                (ConstP m) -> cons m
                (ConstructorP s2 p) -> const s2 p
                (TupleP ps) -> tuple ps
                where
                    var         = case v of (UnitV) -> Just []
                    cons m      = case v of (ConstV n) -> if (n == m) then Just [] else Nothing 
                    const s2 p  = case v of (ConstructorV s1 v) -> if (s1 == s1) then match v p else Nothing
                    tuple ps    = case v of (TupleV vs) ->  if (length vs == length ps) 
                                                            then Just(
                                                                foldl ( \acc (v, p) ->
                                                                    acc ++ (catch (match v p) (\x -> x) [])
                                                                ) [] (zip vs ps)
                                                            )
                                                            else Nothing

-- Question 23. Write a function firstMatch that takes a value and a
-- list of patterns and returns a Maybe [(String, Value)], namely
-- Nothing if no pattern in the list matches or Just lst where lst is
-- the list of bindings for the first pattern in the list that
-- matches. Hint: Your answers from part 4 may be useful here.

firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
firstMatch v list = getFirst (mapPartial (match v) list)

getFirst :: [[(String, Value)]] -> Maybe [(String, Value)]
getFirst [] = Nothing
getFirst l = Just (head l)



div1 :: Int -> Int -> Int
div1 _ 0 = 0
div1 n1 n2 = n1*n2

main = print $ pace ("Yo", 2, 3)

