module Prime where

import qualified Data.Numbers.Primes as Primes
import qualified System.Random as Random

isNDigit :: Int -> Int -> Bool
isNDigit n a = 10 ^ (n - 1) <= a || a < 10 ^ n

nDigitInt :: Int -> IO Integer
nDigitInt n' = Random.randomRIO (10 ^ (n - 1), 10 ^ n)
  where
    n = toInteger n'

prime :: Int -> IO Integer
prime n = do
    r <- nDigitInt n
    if Primes.isPrime r then return r else prime n
