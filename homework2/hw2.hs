
-- CS 456 Homework 2
-- Due date: 02/16/2018 by 9:00PM

-- Name: Vritant Bhardwaj
-- Email: bhardwav@purdue.edu

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
import Data.Char

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
           deriving (Show)

data DSTType =
     DayV Integer
   | MonthV Month
   | Year Integer
   deriving (Show)

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
month :: String -> Maybe([DSTType], String)
namedMonth :: String -> Maybe([DSTType], String)
monthFromNum :: String -> Maybe(DSTType)
day :: String -> Maybe([DSTType], String)
dayAsInt :: String -> DSTType
namedDay :: String -> Maybe([DSTType], String)
year :: String -> Maybe([DSTType], String)
namedYear :: String -> DSTType
constant :: String -> String -> Maybe([DSTType], String)
fseqm :: (String -> Maybe([DSTType], String)) -> (String -> Maybe([DSTType], String)) -> (String -> Maybe([DSTType], String))
try :: (String -> Maybe([DSTType], String)) -> (String -> Maybe([DSTType], String)) -> (String -> Maybe([DSTType], String))
end :: String -> Maybe([DSTType], String)

-- Step Three: Define the semantics of the language.

-- Questions 10-18: Give the semantics of OurTime expressions by
-- writing bodies for the function signatures you defined above.
catch :: Maybe a -> (a -> Maybe b) -> Maybe b -> Maybe b
catch Nothing _ err = err
catch (Just x) f _ = f x

getNextWord :: String -> String -> (String, String)
getNextWord w [] = (w, "")
getNextWord w (l:s) = if(isAlpha l) 
                        then getNextWord (w ++ [l]) s
                        else (w, ([l]++s))

month [] = Nothing
month s =   if(isDigit first) 
            then (
                  if (isDigit second) 
                  then catch (monthFromNum digits) (\x -> Just ([x], tail (tail s))) Nothing
                  else catch (monthFromNum ("0" ++ [first])) (\x -> Just ([x], tail s)) Nothing 
            )     
            else Nothing
            where first = (head s)
                  second = (head (tail s))
                  digits = [first] ++ [second]

monthFromNum s = case s of
                  "01" -> Just(MonthV January)
                  "02" -> Just(MonthV February)
                  "03" -> Just(MonthV March)
                  "04" -> Just(MonthV April)
                  "05" -> Just(MonthV May)
                  "06" -> Just(MonthV June)
                  "07" -> Just(MonthV July)
                  "08" -> Just(MonthV August)
                  "09" -> Just(MonthV September)
                  "10" -> Just(MonthV October)
                  "11" -> Just(MonthV November)
                  "12" -> Just(MonthV December)
                  _    -> Nothing

namedMonth st = case (word) of 
                  "January" -> Just([(MonthV January)], s)
                  "February" -> Just ([(MonthV February)], s)
                  "March" -> Just ([(MonthV March)], s)
                  "April" -> Just ([(MonthV April)], s)
                  "May" -> Just ([(MonthV May)], s)
                  "June" -> Just ([(MonthV June)], s)
                  "July" -> Just ([(MonthV July)], s)
                  "August" -> Just ([(MonthV August)], s)
                  "September" -> Just ([(MonthV September)], s)
                  "October" -> Just ([(MonthV October)], s)
                  "November" -> Just ([(MonthV November)], s)
                  "December" -> Just ([(MonthV December)], s)
                  _ -> Nothing
                  where m@(word, s) = getNextWord "" st

day s =     if (isDigit first) 
            then (
                  if (isDigit second) 
                  then Just ([d2], tail (tail s))
                  else Just ([d1], tail s)
            )     
            else Nothing
            where first = (head s)
                  second = (head (tail s))
                  d1 = dayAsInt [first]
                  d2 = dayAsInt ([first] ++ [second])

dayAsInt s = DayV (read s :: Integer)

namedDay st = case (word) of 
                  "Monday" -> Just([], s)
                  "Tuesday" -> Just([], s)
                  "Wednesday" -> Just([], s)
                  "Thursday" -> Just([], s)
                  "Friday" -> Just([], s)
                  "Saturday" -> Just([], s)
                  "Sunday" -> Just([], s)
                  _ -> Nothing
                  where m@(word, s) = getNextWord "" st

year "" = Nothing
year s =    if (((length s) >= 4) && (isDigit first) && (isDigit second)  && (isDigit third) && (isDigit fourth))
            then Just ([namedYear digits], (tail (tail (tail (tail s)))))
            else Nothing
            where first = (head s)
                  second = (head (tail s))
                  third = (head (tail (tail s)))
                  fourth = (head (tail (tail (tail s))))
                  digits = [first] ++ [second] ++ [third] ++ [fourth]

namedYear s = Year (read s :: Integer)

constantC :: String -> String -> Maybe([DSTType], String)
constantC _ "" = Nothing
constantC c s =   if(c == [(head s)]) 
                  then Just ([], tail s)
                  else Nothing


constant "" s =  Just ([], s)
constant (x:xs) s = catch (constantC [x] s) (\(_, s1) -> constant xs s1) Nothing

fseqm fmt1 fmt2 = (\s -> 
                        catch (fmt1 s) (\(arr1, s1) -> 
                              catch (fmt2 s1) (\(arr2, s2) -> 
                                    Just ((arr1 ++ arr2), s2)
                              ) Nothing
                        ) Nothing
                  )
                  
try fmt1 fmt2 =   (\s -> 
                        catch (fmt1 s) (\x -> Just x) (fmt2 s)
                  )

end s = if (s == "") then Just ([], s) else Nothing

-- Equipped with these functions, you can now write and evaluate
-- OurTime expressions a la ex1 and ex2. Hooray! Using your definition
-- of OurTime, write parsers for the following formats. You can skip
-- the end marker, unless otherwise noted, and treat MM, DD, as
-- representing either one or two digits, per the semantics of OurTime.

-- Question 19
-- Japanese-style dates: #YYYY-MM-DD#
japaneseDate :: String -> Maybe([DSTType], String)
japaneseDate = year `fseqm` (constant "-") `fseqm` month `fseqm` (constant "-") `fseqm` day

-- Question 20
-- Long, American-stye dates: #NamedDay, NamedMonth DD, YYYY#
-- Example: longDate "Monday, June 15, 2009" ≅ Just ([MonthV June, DayV 15, YearV 2009], "")
longDates :: String -> Maybe([DSTType], String)
longDates = namedDay `fseqm` (constant ",") `fseqm` (constant " ") `fseqm` namedMonth `fseqm` (constant " ") `fseqm` day `fseqm` (constant ",") `fseqm` (constant " ") `fseqm` year


-- Question 21
-- Variant dates: #MM-DD-YYYY (en-US) # | #DD-MM-YYYY (fr-FR) # | #YYYY-DD-MM (ja-JP) #
-- Example variantDate "12-25-2018 (en-US)" ≅ Just ([MonthV December, DayV 25, YearV 2018], "")
-- Example variantDate "12-25-2018 (fr-FR)" ≅ None
-- Example variantDate "12-25-2018 (ja-JP)" ≅ None



enUS :: String ->  Maybe([DSTType], String)
enUS = month `fseqm` (constant "-") `fseqm` day `fseqm` (constant "-") `fseqm` year `fseqm` (constant " (en-US)")

frFr :: String ->  Maybe([DSTType], String)
frFr = day `fseqm` (constant "-") `fseqm` month `fseqm` (constant "-") `fseqm` year `fseqm` (constant" (fr-FR)")

jaJP :: String ->  Maybe([DSTType], String)
jaJP = year `fseqm` (constant "-") `fseqm` day `fseqm` (constant "-") `fseqm` month `fseqm` (constant" (ja-JP)")

variantDate :: String -> Maybe([DSTType], String)
variantDate = try (try enUS frFr) jaJP

-- Question 22
-- A sequence of variant dates: #[VariantDates]+ # (Here, the end marker is important!)
-- Example:
-- variantDates "12-25-2018 (en-US), 25-11-2017 (fr-FR), 1918-4-11 (ja-JP)" ≅
-- Just ([MonthV December, DayV 25, YearV 2018, MonthV November, DayV 25, YearV 2017, YearV 1918, MonthV April, DayV 11], "")

variantDates :: String -> Maybe ([DSTType], String)
variantDates =    (\s -> 
                        catch (variantDate s) 
                              (\(arr1, s1) -> 
                                    if(s1 == "")
                                    then  Just (arr1, s1)
                                    else
                                          catch (parseConstant s1) 
                                                (\(arr2, s2) -> 
                                                      catch (variantDates s2) 
                                                      (\(arr3, s3) -> 
                                                            Just ((arr1 ++ arr3), s3)
                                                      ) 
                                                      Nothing
                                                ) 
                                                Nothing
                              ) 
                              Nothing
                  )

parseConstant :: String -> Maybe ([DSTType], String)
parseConstant = constant ", "

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

instance Show Pattern where
      show (WildCardP)          =  "*"
      show (UnitP)              = "()"
      show (VariableP s)        = "x_" ++ s
      show (ConstP d)           = "c_" ++ (show d)
      show (TupleP ps)          = "(" ++ 
                                    (foldl 
                                          (\acc (p, i) -> 
                                                if (i == (length ps))
                                                then acc ++ (show p) ++ " "
                                                else acc ++ (show p) ++ ", "
                                          ) "" (
                                                zip ps [1..((length ps))]
                                          )
                                    ) 
                                  ++ ")"
      show (ConstructorP s p)   = s ++ " " ++ (show p)

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

instance Conversion FancyRunRecord RunRecord where
      (~!) (FancyRunRecord l s t d) = (l, s, t)

instance Conversion RunRecord RunRecord where
      (~!) r = r

pace :: RunRecord -> Float
pace (_, dist, time) = dist / time

fancyPace :: FancyRunRecord -> Float
fancyPace fr = pace ((~!) fr)

-- Question 26: Define an instance showing how to lift an instance of
-- YesNo a to an instance of Coercion a Bool.

instance YesNo a => Conversion a Bool where
      (~!) a = yesno a

-- Question 27: Do the same to build an instance of Conversion Integer a
-- from Num a.

instance Num a => Conversion Integer a where
      (~!) a = fromInteger a

-- Question 28: Use a similar approach to define generic
-- conversions between the polymorphic types Maybe a and Maybe b,
-- given a conversion from a to b.

instance {-# OVERLAPPING #-} (Conversion a b) => Conversion (Maybe a) (Maybe b) where
       (~!) (Nothing) = Nothing
       (~!) (Just a) = Just ((~!) a)

-- Question 29: Do the same for lists.
instance {-# OVERLAPPING #-}  (Conversion a b) => Conversion [a] [b] where
      (~!) [] = []
      (~!) (x:xs) = ((~!) x) : ((~!) xs)

-- instance (Conversion a b) => Conversion (Maybe [a]) (Maybe [b]) where
--       (~!) Nothing = Nothing
--       (~!) (Just []) = Just []
--       (~!) (Just (x:xs)) = Just (((~!) x) : ((~!) xs))

-- Question 30: Use the conversion typeclass to write a generic
-- magicMap operation that automagically converts the elements of a
-- list before mapping the supplied operator.

magicMap :: (Conversion a b) => (b -> c) -> [a] -> [c]
magicMap f [] = []
magicMap f as = map f $ (~!) as

-- Question 31: The instances of the Functor typeclass define an fmap
-- operator which shows how to map a function over the values of a
-- polymorphic data type. Use this with the the conversion typeclass
-- to show how to generically convert between container types.

instance (Functor f, Conversion a b) => Conversion (f a) (f b) where
      (~!) a = ((~!) a)

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
-- 
-- Theorem: ∀ f l1 l2. map f (l1 ++ l2) ≅ map f l1 ++ map f l2
-- Case l1 = []
-- = map f ([] ++ l2)                                   (LHS)
-- = map f l2                                           (By Evaluation)
-- = map f l2 ++ []                                     (By Definition of lists)
-- = map f l2 ++ map f []                               (By fold definition of map)
-- = True

-- Case l1 = l11:l1s
-- = map f (l1 ++ l2)                                   (LHS)
-- = map f ([l11, l12, .., l1n, l21, l22, .., l2m])     (By definition of ++)
-- = map f (l11 : [l12,..,l1n, l21,..,l2m]])            (By definition of list)
-- = case l11 : [l12,..,l1n, l21,..,l2m]] of {[]=[]; (x:xs) -> (f x) : map f xs)} (By unfold definition of map)
-- = (f l11) : map f [l12,..,l1n, l21,..,l2m]]          (By evaluation)
-- ...
-- = (f l11) : (f l22) : .. : (f l1n) : map f [l21, .. l2m] (By repeating unfold definition)
-- = [f l11, f l12, .. flm] ++ map f l2                 (By definition of :)
-- = map f l1 ++ map f l2                               (By fold definition of map)
-- Qed.


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
-- Case l = []
-- = map (\x -> x + 1) []                             (LHS)
-- = case [] of {[]=[]; (x:xs) -> (f x) : map f xs)}  (By unfold definition of map)
-- = []                                               (By evaluation)
-- = True

-- Case l = y:ys
-- = map (\x > x + 1) (y:ys)                          (LHS)
-- = (y + 1) : (map (\x -> x + 1) ys)                 (By definition of map)
-- = (1 + y) : (map (\x -> x + 1) ys)                 (By associativity)
-- Qed.

-- Question 36:
-- Theorem:  ∀ l. incList1 l ≅ incList3 l.
-- Case l = []
-- = map (\x -> x + 1) []                              (LHS)
-- = case [] of {[]=[]; (x:xs) -> (f x) : map f xs)}   (By unfold definition of map)
-- = []                                                (By evaluation of map f [])
-- = True

-- Case l = y1:ys
-- = (1 + y1) : incList3 ys                           (RHS)
-- = (1 + y1) : incList3 y2:ys                        (By definition of :)
-- = (1 + y1) : (1 + y2) : incList3 y3:ys             (By evaluation)
-- = (1 + y1) : (1 + y2) : ... (1 + yn)               (By repeated evaluation)
-- = [(1 + y1), (1 + y2),.., (1 + yn)]                (By definition of :)
-- = map (\x -> x + 1) [y1:ys]                        (By fold definition of map)
-- = map (\x -> x + 1) l                              (By inductive hypothesis)
-- Qed.

-- Question 37: In one sentence, what's the key difference between the proofs for questions 35 and 36?
-- In question 35, the two functions are almost identical by a step, and can thus be proven by an equational proof, 
-- but the second proof requires and inductive hypothesis along with an inductive proof.

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
-- Will not work as types don't confer. 
-- zip' takes in type [a] -> [b]
-- but unzip' returns a tuple ([a], [b])

-- Question 39: Prove or disprove (by giving an input that's a counterexample)
-- Theorem?:  ∀ l1 l2. unzip' (zip' l1 l2) ≅ (l1, l2).
--Will not work
-- Counter example : 
-- l1 = [1, 2, 3]
-- l2 = [1, 2]
-- The theorem won't apply as the result would be ([1, 2], [1, 2]) which is not equal to (l1, l2)

main = print $  "Yo"
