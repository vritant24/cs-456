-- CS 456 Homework 2
-- Due date: 02/16/2018 by 9:00PM

-- Name: YOUR NAME HERE
-- Email: YOUR EMAIL HERE

-- ASSIGNMENT INSTRUCTIONS:
-- ========================================================================
-- All questions have to be completed. Before submitting, make sure
-- that running 'ghc hw2.hs' in the shell completes without errors.
-- If your homework does not compile, it will not be graded! If an
-- incomplete answer to a question is causing ghc to fail, comment it
-- out.

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- Submit all of your files via turnin using the instructions here (windows
-- users will perform a similar incantation):
-- https://www.cs.purdue.edu/homes/cs177/turnin-from-home-macos.html
-- Use cs456 for the class name and hw2 for the project name:
-- ssh <your username>@lore.cs.purdue.edu /usr/local/bin/turnin -c cs456 -p hw2 <your files>

-- These pragma is needed for part 2
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Part 1: Shallowly Embedded DSLs

-- For this portion of the homework, you'll be writing a simple domain
-- specific language, called OurTime*, for processing date and time
-- strings. The implementation technique will be similar to that of
-- the OurSQL language we saw on 1/25: a 'program' in this language
-- will be a function with the type signature :: String -> Maybe
-- ([DSTType], String). Intuitively, applying such a program
-- to a string will produce either a dictionary of values and the
-- unconsumed portion of the string or None to indicate the string
-- failed to parse.

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December

data DSTType =
     DayV Integer
   | MonthV Month
   | Year Integer

-- As an example, suppose we have an OurTime program, mdy,
-- representing the format MM/DD/YYYY. Here are some example output values:
-- mdy "03/20/2014" ≅ Just ([MonthV May, DayV 20, Year 2014],  [ ])
-- mdy "03/Crazy!/2014" ≅ Nothing
-- mdy "03/60/2014 WackyStuff" ≅ Just ([MonthV May, DayV 20, Year 2014],  " WackyStuff")
-- mdy "03/29   /2014" ≅ Nothing

-- As the third example shows, there's no notion of sanity checks on
-- the values of the day portion of the date; OurTime only does
-- syntactic checks. You can imagine we might write such a sanity
-- check after the fact. Also, as the fourth example demonstrates,
-- whitespace counts.

-- Step One: Here's the definition of the syntax of the language, and their intuitive meanings:
-- month : One or two digits: [0-9][0-9] | [0-9]
-- namedMonth : One of "January", "February", "March", ...
-- day : One or two digits: [0-9][0-9] | [0-9]
-- namedDay : One of "Monday", "Tuesday", "Wednesday", ...
-- year: four digits: [0-9][0-9][0-9][0-9] | [0-9][0-9][0-9] | ...
-- constant x : Matches precisely the string x
-- fseqm fmt1 fmt2 : Sequences two formats
-- try fmt1 fmt2 : Tries to parse with the first format; if that fails, try the second.
-- end : Only matches the EmptyString.

-- Thus, we could express the format for the above example as the
-- following OurTime program:
-- ex1 = fseq month (fseq (constant "/") (fseq day (fseq (constant "/") (fseq year end))))

-- Note: you can use backticks to infix-ify an operator with an
-- alphanumeric name , allowing us to write this function in the
-- slightly more appealing:
-- ex2 = month `fseq` day `fseq` (constant "/") `fseq` year `fseq` end

-- Step Two: What kinds of expressions make sense?
-- We now want to use Haskell's type system to rule out nonsensical
-- OurTime programs.

-- Questions 1-9: Define the type signatures for the nine pieces of
-- syntax for OurTime. Defining a type alias, OurTimeType, for the
-- general type of OurTime expressions may make for prettier signatures:
-- month ::
-- namedMonth ::
-- day ::
-- namedDay ::
-- year ::
-- constant x ::
-- fseqm fmt1 fmt2 ::
-- try fmt1 fmt2 ::
-- end ::

-- Step Three: Define the semantics of the language.

-- Questions 10-18: Give the semantics of OurTime expressions by
-- writing bodies for the function signatures you defined above.
-- month =
-- namedMonth =
-- day =
-- namedDay =
-- year =
-- constant x =
-- fseqm fmt1 fmt2 =
-- try fmt1 fmt2 =
-- end =

-- Equipped with these functions, you can now write and evaluate
-- OurTime expressions a la ex1 and ex2. Hooray! Using your definition
-- of OurTime, write parsers for the following formats. You can skip
-- the end marker, unless otherwise noted, and treat MM, DD, as
-- representing either one or two digits, per the semantics of OurTime.

-- Question 19
-- Japanese-style dates: #YYYY-MM-DD#
-- japaneseDate

-- Question 20
-- Long, American-stye dates: #NamedDay, NamedMonth DD, YYYY#
-- Example: longDate "Monday, June 15, 2009" ≅ Just ([MonthV June, DayV 15, YearV 2009], "")
-- longDates

-- Question 21
-- Variant dates: #MM-DD-YYYY (en-US) # | #DD-MM-YYYY (fr-FR) # | #YYYY-DD-MM (ja-JP) #
-- Example variantDate "12-25-2018 (en-US)" ≅ Just ([MonthV December, DayV 25, YearV 2018], "")
-- Example variantDate "12-25-2018 (fr-FR)" ≅ None
-- Example variantDate "12-25-2018 (ja-JP)" ≅ None
-- variantDates

-- Question 22
-- A sequence of variant dates: #[VariantDates]+ # (Here, the end marker is important!)
-- Example:
-- variantDates "12-25-2018 (en-US), 25-11-2017 (fr-FR), 1918-4-11 (ja-JP)" ≅
-- Just ([MonthV December, DayV 25, YearV 2018, MonthV November, DayV 25, YearV 2017, YearV 1918, MonthV April, DayV 11], "")

-- *I found out OurTime is the name of an over-50s dating site after
-- creating this homework; there is no clever relationship between
-- this EDSL and that website (unfortunately).

-- Part 2: Typeclasses

-- Question 23: Recall that if you omit a "deriving (Show)" from a
-- datatype definition, you can define your own pretty printing
-- function for that datatype by defining the appropriate typeclass
-- instance.  Define such a pretty printer for the Pattern datatype
-- from the first homework.

data Pattern =
     WildCardP          -- WildcardP prints as "* "
     | VariableP String -- VariableP s prints as "x_s "
     | UnitP            -- UnitP prints as "()"
     | ConstP Int       -- ConstP i prints as "c_i "
     | TupleP [Pattern] -- Tuple [x,y,z] prints as "(XX, YY, ZZ) ", where XX is the pretty printing of x, etc.
     | ConstructorP String Pattern -- Constructor c x prints as "c XX ", where XX as above

-- So, the pretty-printed version of ConstructorP "Plus" (TupleP [ConstructorP "Minus" (TupleP [WildCardP, VariableP "X"]), ConstructorP "Plus" (TupleP [ConstP 1, VariableP "Y"])])
-- would be: "Plus (Minus (* , x_X ), Plus (c_1 , x_Y ) )"

-- In the typeclasses lecture (1/23), we defined the YesNo typeclass
-- whose instances showed how to transform a value of a type into a
-- Boolean. The class' yesno operation was an example of an explicit
-- type conversion function:

class YesNo a where
      yesno :: a -> Bool

-- You could also imagine wanting to define a more general conversion
-- operation (~!) for converting between values of two arbitrary
-- types. Given such an operation you could, for example, explicitly
-- "cast" an extended RunRecord into the RunRecords in order to use
-- them with the functions you wrote for the last homework.

type RunRecord = (String, Float, Float)
data FancyRunRecord = FancyRunRecord { loc :: String, dist :: Float, time :: Float, heartRate :: Float}
-- fancyPace :: FancyRunRecord -> Float
-- fancyPace fr = pace ((~!) fr)

-- The downside is that you need to insert explict "casts", but oh well.

-- Question 24: Define a typeclass Conversion a b, that includes a
-- single (~!) for mapping values of type a to values of type b.  FYI:
-- Creating typeclasses with multiple type parameters requires the
-- special pragma that's at the beginning of the assignment.

class Conversion a b where
      (~!) :: a -> b

-- Question 25: Define an instance of Conversion between RunRecords
-- and FancyRunRecords, and use it to define a fancyPace function as
-- above (you'll need to copy your pace function over).

-- Question 26: Define an instance showing how to lift an instance of
-- YesNo a to an instance of Coercion a Bool.

-- Question 27: Do the same for Num a.

-- Question 28: Use a similar approach to define generic
-- conversions between the polymorphic types Maybe a and Maybe b,
-- given a conversion from a to b.

-- Question 29: Do the same for lists.

-- Question 30: Use the conversion typeclass to write a generic
-- magicMap operation that automagically converts the elements of a
-- list before mapping the supplied operator.

-- magicMap :: (Conversion a b) => (b -> c) -> [a] -> [c]
-- magicMap f as = ...

-- Question 31: The instances of the Functor typeclass define an fmap
-- operator which shows how to map a function over the values of a
-- polymorphic data type. Use this with the the conversion typeclass
-- to show how to generically convert between container types.
-- instance (Functor f, Conversion a b) => Conversion (f a) (f b) where ...

-- Part 3: Equational Reasoning:
-- Prove the following theorems using induction and equational reasoning.
-- Write your proofs in a comment following each question; here is an example
-- formatting of one of the proofs from class:
-- Theorem: ∀ n. even (n*2) ≅ True.
-- Proof by induction on n.
-- Case n = 0:
--   even (0*2)                                 (left-hand side of equation)
-- ≅ even 0                                     (left-hand side of equation)
-- ≅ case 0 of 0 -> True | …                    (by definition of even)
-- ≅ True                                       (by definition + symmetry)

-- Case n = 1 + m: Given that  even m*2  ≅ True (induction hypothesis),
--   even ((1+m)*2)                             (left-hand side of equation)
-- ≅ even (2+m*2)                               (evaluation +  substitution)
-- ≅ case (2 + m*2) of 0 -> True | …            (by definition of even)
-- ≅ 2 + even (m*2) - 2                         (by evaluation)
-- ≅ even (m*2)                                 (by math)
-- ≅ True                                       (by IH)
-- Qed.

-- Question 34:
-- Theorem: ∀ f l1 l2. map f (l1 ++ l2) ≅ map f l1 ++ map f l2

-- Question 35-37: Given the following definitions,
incList1 :: [Int] -> [Int]
incList1 l = map (\x -> x + 1) l

incList2 :: [Int] -> [Int]
incList2 [] = []
incList2 (x : xs) = (1 + x) : map (\x -> 1 + x) xs

incList3 :: [Int] -> [Int]
incList3 [] = []
incList3 (x : xs) = (1 + x) : incList3 xs

-- Question 35:
-- Theorem:  ∀ l. incList1 l ≅ incList2 l.

-- Question 36:
-- Theorem:  ∀ l. incList1 l ≅ incList3 l.

-- Question 37: In one sentence, what's the key difference between the proofs for questions 35 and 36?

-- Questions 38-39: Given the following definitions,

zip' :: [a] -> [b] -> [(a,b)]
zip' []     bs    = []
zip' as    []     = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a, b) : abs) = let (as, bs) = unzip' abs in
                            (a : as, b : bs)

-- Question 38: Prove or disprove (by giving an input that's a counterexample)
-- Theorem?:  ∀ l. zip' (unzip' l) ≅ l.

-- Question 39: Prove or disprove (by giving an input that's a counterexample)
-- Theorem?:  ∀ l1 l2. unzip' (zip' l1 l2) ≅ (l1, l2).

main = print $  "Yo"
