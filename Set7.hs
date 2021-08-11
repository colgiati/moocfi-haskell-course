-- Exercise set 7

module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance a) (Time b) = Velocity (a / b)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity a) (Time b) = Distance (a * b)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a]
  deriving (Show,Eq)

-- emptySet is a set with no elements
emptySet :: Set a
emptySet = Set []

-- member tests if an element is in a set
member :: Eq a => a -> Set a -> Bool
member k (Set []) = False
member k (Set (x:xs)) = (k == x) || member k (Set xs)

-- add a member to a set
add :: (Ord a) => a -> Set a -> Set a
add k (Set []) = Set [k]
add k (Set xs) = Set (ins k xs)
  where ins k [] = [k]
        ins k (x:xs)
          | k < x = k : (x : xs)
          | k == x = x : xs
          | otherwise = x : ins k xs

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

data State = Start | Egged | Floured | Sugared | FlourSugared | Mixed | Error | Finished
  deriving (Eq,Show)

step :: State -> Event -> State
step Start AddEggs = Egged
step Egged AddFlour = Floured
step Egged AddSugar = Sugared
step Sugared AddFlour = FlourSugared
step Floured AddSugar = FlourSugared
step FlourSugared Mix = Mixed
step Mixed Bake = Finished
step Finished _ = Finished
step _ _ = Error

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

average :: Fractional a => NonEmpty a -> a
average (x:| []) = x
average (x:|xs) = (x + sum xs) / fromIntegral (1 + length xs)

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x:| []) = x:|[]
reverseNonEmpty (x:| xs) = last (x:xs) :| tail (reverse (x:xs))

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  (<>) (Distance a) (Distance b) = Distance (a + b)

instance Semigroup Velocity where
  (<>) (Velocity a) (Velocity b) = Velocity (a + b)

instance Semigroup Time where
  (<>) (Time a) (Time b) = Time (a + b)

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?

-- data Set a = Set [a]
--   deriving (Show,Eq)

instance (Ord a) => Semigroup (Set a) where
  (<>) (Set x) (Set y) = Set (merge x y)

instance (Ord a) => Monoid (Set a) where
  mempty = Set []

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  | x == y = x : merge xs ys
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
  
------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
  deriving Show

compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i + j
compute1 (Subtract1 i j) = i - j
compute1 (Multiply1 i j) = i * j

show1 :: Operation1 -> String
show1 (Multiply1 a b) = show a ++ "*" ++ show b
show1 (Add1 a b) = show a ++ "+" ++ show b
show1 (Subtract1 a b) = show a ++ "-" ++ show b

data Add2 = Add2 Int Int
  deriving Show
data Subtract2 = Subtract2 Int Int
  deriving Show
data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

instance Operation2 Add2 where
  compute2 (Add2 i j) = i + j
  show2 (Add2 a b) = show a ++ "+" ++ show b

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i - j
  show2 (Subtract2 a b) = show a ++ "-" ++ show b

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i * j
  show2 (Multiply2 a b) = show a ++ "*" ++ show b

------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String    -- contains at least one of given characters
  | DoesNotContain String  -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement  -- or'ing
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed pw (MinimumLength x) = length pw >= x
passwordAllowed pw (ContainsSome x) = contains pw x
passwordAllowed pw (DoesNotContain x) = not (passwordAllowed pw (ContainsSome x))
passwordAllowed pw (And x y) = passwordAllowed pw x && passwordAllowed pw y
passwordAllowed pw (Or x y) = passwordAllowed pw x || passwordAllowed pw y

contains :: Eq t => [t] -> [t] -> Bool
contains _ [] = False
contains xs (y:ys) = cont xs y || contains xs ys
  where cont [] _ = False 
        cont (x:xs) y = x == y || cont xs y

------------------------------------------------------------------------------
-- Ex 10: a DSL for simple arithmetic expressions with addition and
-- multiplication. Define the type Arithmetic so that it can express
-- expressions like this. Define the functions literal and operation
-- for creating Arithmetic values.
--
-- Define two interpreters for Arithmetic: evaluate should compute the
-- expression, and render should show the expression as a string.
--
-- Examples:
--   evaluate (literal 3) ==> 3
--   render   (literal 3) ==> "3"
--   evaluate (operation "+" (literal 3) (literal 4)) ==> 7
--   render   (operation "+" (literal 3) (literal 4)) ==> "(3+4)"
--   evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> 6
--   render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> "(3*(1+1))"
--

data Arithmetic = Val Integer | Op String Arithmetic Arithmetic
  deriving Show

literal :: Integer -> Arithmetic
literal = Val

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = Op

evaluate :: Arithmetic -> Integer
evaluate (Val x) = x
evaluate (Op "+" x y) = (+) (evaluate x) (evaluate y)
evaluate (Op "*" x y) = (*) (evaluate x) (evaluate y)
evaluate _ = 0

render :: Arithmetic -> String
render (Val x) = show x
render (Op a x y) = "(" ++ render x ++ a ++ render y ++ ")"
