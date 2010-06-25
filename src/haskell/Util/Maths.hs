-- | Mathematical utility functions.
module Util.Maths (percentage,
                   roundUp) where

-- | Calculate the first argument as a percentage of the second.
percentage :: Int -> Int -> Float
percentage num denom = fromIntegral (num * 100) / fromIntegral denom

-- | Round-up integer n to the nearest m.  E.g. roundUp 67 20 = 80.
roundUp :: Int -> Int -> Int
roundUp n m = (n + m - 1) `div` m * m
