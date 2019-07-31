module Euclid
    ( EuclideanNecklace
    , EuclideanRhythm
    , euclid
    , superEuclid
    , EuclidModifier
    , shiftL
    , shiftR
    , monoizeDistancesL
    , monoizeDistancesR
    , euclidToMode
    )
where

import Data.List (sort)

type EuclideanNecklace = [Int] -- distances

type EuclideanRhythm = (Int, EuclideanNecklace) -- offset and distances

{-| @distribEvenly'@ takes a list of lists of something (the accumulator) and a
    list of something (the remainder) and returns the accumulator with the
    remainder evenly distributed within it. For example, calling
    `distribEvenly' [[2],[2],[2]] [1,1]` returns `[2,1,2,1,2]`.
-}
distribEvenly' :: [[a]] -> [a] -> [a]
-- If the remainder is empty, concatenate and return the accumulator.
distribEvenly' acc []         = concat acc
-- If the remainder has only one element, tack it onto the end of the
-- concatenated accumulator and return the result.
distribEvenly' acc (rem : []) = concat acc ++ [rem]
distribEvenly' acc rem =
    let
        -- Distribute all the remainder values over the accumulator (as far as
        -- possible). For the example above, we'd get `[[2,1], [2,1]]` (as `zip`
        -- always creates a list of the length of the shorter input list).
        newAcc          = map (\(xs, y) -> xs ++ [y]) $ zip acc rem

        -- Calculate what's left of the old remainder (in our example, `[]`).
        remnantOfOldRem = drop (length newAcc) rem
        -- Calculate the new, higher-level remainder (in our example, `[2]`).
        newRem          = drop (length newAcc) acc
    in if length remnantOfOldRem > 1
       -- If there's more than 1 element left of the old remainder, we'll keep
       -- going with that one, distributing it's values in the accumulator.
        then distribEvenly' newAcc remnantOfOldRem
       -- If not, we'll do the same thing recursively at a higher level. In
       -- other words, if there are any odd subdivisions, we need to distribute
       -- those evenly, too.
        else concat $ distribEvenly' (map pure newAcc) newRem ++ [remnantOfOldRem]

{-| @distribEvenly@ takes a list of ordered numbers (of two different values
    only) and returns them evenly distributed over the indices. For instance,
    `[2,2,2,1,1]` or `[1,1,2,2,2]` returns `[2,1,2,1,2]`.
-}
distribEvenly :: (Num a, Ord a) => [a] -> [a]
distribEvenly xs =
    let
        maxVal               = foldl max 0 xs -- maximum value from the list

        -- Split the list into two, one containing all the higher values
        -- (`initAcc`) and one containing the lower values (`remainder`).
        (remainder, initAcc) = span (< maxVal) $ sort xs
    in 
        -- Now we nest each of the high values into its own list. We then call
        -- `distribEvenly'`, which will intersperse the lower numbers in these
        -- lists. For the example above, we would call it with
        -- `[[2],[2],[2]]` and `[1,1]`.
       distribEvenly' (map pure initAcc) remainder

{-| @euclid@ takes a number of pulses `k` and number of points `n` and returns a
    Euclidean rhythm represented as a list of `k` distances.
-}
euclid :: Int -> Int -> EuclideanNecklace
euclid k n =
    let
        quot      = n `div` k
        rem       = n `mod` k

        -- We want k pulses, hence we will have k distances. The distances, if
        -- they are to be as evenly distributed as possible, need to be at least
        -- as high as the quotient. E.g. for E(3,8) we get 222 here.
        base      = replicate k quot

        -- We want n points, so the total sum of all distances should be n.
        -- Hence we need `n - sum base` additional distance units. We'll
        -- distribute these evenly over the first `n - sum base` pulses.
        -- Continuing the example, `8 - (2 + 2 + 2) = 2`, giving us 332. These
        -- are the final distances that we want to return, only now they are not
        -- evenly distributed. To achieve that end, we call `distribEvenly`.
        distances = zipWith (+) base (replicate rem 1 ++ replicate (k - rem) 0)
    in distribEvenly distances

{-| @superEuclid@ takes a list of numbers of pulses `ks` and number of points
    `n` and returns a composite Euclidean rhythm represented as a list of `k`
    distances.
-}
superEuclid :: [Int] -> Int -> EuclideanNecklace
superEuclid ks n =
    let scheme = euclid (length ks) n -- Divides cycle into (length ks) parts.
                                      in zip ks scheme >>= \(k, subN) -> euclid k subN

type EuclidModifier = [Int] -> [Int]

shift' :: (Int -> Int -> Int) -> EuclidModifier
shift' _ []       = []
shift' f (x : []) = [f x (-1)]
shift' f (x : xs) = (x : shift' f xs)

shift :: (Int -> Int -> Int) -> EuclidModifier
shift f (x : xs) = filter (> 0) $ shift' f (f x 1 : xs)

shiftR :: EuclidModifier
shiftR = shift (+)

shiftL :: EuclidModifier
shiftL = shift (-)

{-| @monoizeDistancesL@ takes a list of Euclidean rhythms and makes it so that
    only one of them contains a pulse at any one time (with priority given to
    the fist rhythm of the list.)
-}
monoizeDistancesL :: [EuclideanRhythm] -> [EuclideanRhythm]
monoizeDistancesL = reverse . monoizeDistancesR . reverse

{-| @monoizeDistancesR@ takes a list of Euclidean rhythms and makes it so that
    only one of them contains a pulse at any one time (with priority given to
    the last rhythm of the list.)
-}
monoizeDistancesR :: [EuclideanRhythm] -> [EuclideanRhythm]
monoizeDistancesR []       = []
monoizeDistancesR (r : []) = [r]
monoizeDistancesR (r@(o, ds) : rs) =
    let
        absPositions o ys = map (+ o) $ take (length ys) $ scanl (+) 0 ys
        forbiddenPositions = rs >>= uncurry absPositions
        goodPositions      = uncurry absPositions r >>= \dPos -> if elem dPos forbiddenPositions then [] else [dPos]
    in if length goodPositions == 0
        then monoizeDistancesR rs
        else
            let
                initialRestDur = head goodPositions
                ps             = goodPositions ++ [sum ds] -- final position is end of cycle
                monoDistances  = drop 2 $ map snd $ scanl (\(oldP, _) p -> (p, p - oldP)) (0, 0) ps
                monoRhythm     = (initialRestDur, monoDistances)
            in monoRhythm : monoizeDistancesR rs

{-| @euclidToMode@ takes a list of distances and returns a list of intervals
    starting at 0.
-}
euclidToMode :: [Int] -> [Int]
euclidToMode ds = take (length ds) $ foldr (\d mode@(m : _) -> ((m - d) `mod` 12 : mode)) [0] ds
