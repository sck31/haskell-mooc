module Repeat where

-- Some recurstion examples from lecture 2
repeatString :: Int -> String -> String
repeatString n str = repeatHelper n str ""

repeatHelper :: Int -> String -> String -> String
repeatHelper n str result = if (n==0)
                            then result
                            else repeatHelper (n-1) str (result++str)

-- Same as above, but with pattern matching on the helper
repeatString2 :: Int -> String -> String
repeatString2 n str = repeatHelper2 n str ""

repeatHelper2 :: Int -> String -> String -> String
repeatHelper2 0 _   result = result
repeatHelper2 n str result = repeatHelper (n-1) str (result++str)

-- Fibonacci numbers, slow version
fibonacci_slow :: Int -> Int
fibonacci_slow 1 = 1
fibonacci_slow 2 = 1
fibonacci_slow n = fibonacci_slow (n-2) + fibonacci_slow (n-1)

-- fibonacci numbers, fast version
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' _ b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)


{-# LANGUAGE BangPatterns #-}
-- Fibonacci numbers, forcing strict
fibonacci_strict :: Integer -> Integer
fibonacci_strict n = fibonacci_strict' 0 1 n

fibonacci_strict' :: Integer -> Integer -> Integer -> Integer
fibonacci_strict' _ b 1 = b
fibonacci_strict' a !b n = fibonacci_strict' b (a+b) (n-1)

