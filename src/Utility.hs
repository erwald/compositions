module Utility
    ( nth
    , curry3
    , uncurry3
    , rotateL
    , rotateR
    , spanUntilClosing
    )
where

{- | Returns the value at the given index of the given list, or Nothing if the
    index was out of bounds. --}
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _ ) = Just x
nth n (_ : xs) = nth (n - 1) xs

{-| Converts an uncurried function to a curried function. -}
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

{-| Converts a curried function to a function on a triple. -}
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

rotateL :: Int -> [a] -> [a]
rotateL _ [] = []
rotateL n xs = zipWith const (drop (n `mod` length xs) (cycle xs)) xs

rotateR :: Int -> [a] -> [a]
rotateR n xs = rotateL (length xs - n) xs

spanUntilClosing' :: Eq a => Int -> ([a], [a]) -> a -> a -> [a] -> ([a], [a])
spanUntilClosing' _ (hd, tl) _ _ [] = (hd, tl)
spanUntilClosing' depth (hd, tl) opener closer (x : xs)
    | x == opener = spanUntilClosing' (depth + 1) (hd ++ [x], tl) opener closer xs
    | x == closer = if (depth - 1) == 0
        then (hd, tl ++ xs)  -- skip final closing bracket
        else spanUntilClosing' (depth - 1) (hd ++ [x], tl) opener closer xs
    | otherwise = spanUntilClosing' depth (hd ++ [x], tl) opener closer xs

{-| Given a string that begins with an opening bracket, returns a tuple with, first, the contents 
within those brackets, and second, the tail after the matching closing bracket. -}
spanUntilClosing :: String -> (String, String)
spanUntilClosing []         = ([], [])
spanUntilClosing ('(' : xs) = spanUntilClosing' 1 ([], []) '(' ')' xs
spanUntilClosing ('[' : xs) = spanUntilClosing' 1 ([], []) '[' ']' xs
spanUntilClosing ('{' : xs) = spanUntilClosing' 1 ([], []) '{' '}' xs
spanUntilClosing xs         = ([], xs)
