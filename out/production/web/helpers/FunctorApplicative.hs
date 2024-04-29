{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module FunctorApplicative where

import Control.Applicative (liftA3)

-- The <$> operator, also known as fmap
-- applies a function to the value(s) inside a functor (or any applicative context).

-- Multiply each element in a list by 2
numbers :: [Integer]
numbers = [1, 2, 3]

doubled :: [Integer]
doubled = (* 2) <$> numbers

-- Applying a function to a Maybe value
maybeValue :: Maybe Integer
maybeValue = Just 5

squared :: Maybe Integer
squared = (^ 2) <$> maybeValue -- Just 25

-- Applying a function to an optional value
maybeName :: Maybe String
maybeName = Just "John"

greeting :: Maybe [Char]
greeting = ("Hello, " ++) <$> maybeName -- Just "Hello, John"

-- <*> (Applicative)
-- The <*> operator applies a function inside an applicative context to a value inside another applicative context.
--  It allows for sequencing computations and applying functions to multiple values that are in a context.

-- Combining two Maybe values with a function
maybeAdd = (+) <$> Just 3 <*> Just 4 -- Just 7
-- Combining two lists with a function

combined = (*) <$> [1, 2] <*> [10, 20] -- [10, 20, 20, 40]
-- Performing IO actions and combining their results
-- readAndPrint = (++) <$> getLine <*> getLine

-- Using <*> with a curried function
combineThree = liftA3 (\x y z -> x + y + z) (Just (1 :: Integer)) (Just (2 :: Integer)) (Just (3 :: Integer))

--NOTE
--Maybe (a0 -> a0) ( a function or variable)
--(a0 -> a0): This is a type representing a function that takes an argument of type a0 and returns a result of the same type a0.

--Maybe (a0 -> a0): This means a Maybe value that can either contain:
--Just f, where f is a function of type (a0 -> a0), or
--Nothing, representing no value or absence of a function.

--Just (\x -> x + 1)
--Here, (\x -> x + 1) is a function of type Integer -> Integer, and it's wrapped in a Just, resulting in a Maybe (Integer -> Integer).
