-- | Utility functions for working with tuples.
module Util.Tuple (fst3,
                   snd3,
                   trd3,
                   pair) where

-- | Look-up the first item in a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Look-up the second item in a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Look-up the third item in a 3-tuple.
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- | Create a tuple from a single value by applying two separate functions to the value.
pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)
