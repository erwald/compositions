import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Control.Monad
import Data.Maybe (isNothing, isJust)

import Euclid
import Utility

main :: IO ()
main = hspec $ do
    describe "in Euclid.hs" $ do
        context "euclid" $ do
            it "should return k distances" $ property prop_euclidLen
            it "distances should sum up to n" $ property prop_euclidSum
        context "superEuclid" $ do
            it "should return # of distances equal to sum of ks" $ property prop_superEuclidLen
            it "distances should sum up to n" $ property prop_superEuclidSum
    describe "in Utility.hs" $ do
        context "nth" $ do
            it "returns Nothing for negative indices" $ property prop_nthNothingForNegativeIndices
            it "returns Nothing for out-of-bounds indices" $ property prop_nthNothingForOutOfBoundsIndices
            it "returns something for in-bounds indices" $ property prop_nthReturnsValueForIndicesInBounds
        context "rotateL" $ do
            it "retain length of original lists" $ property prop_rotateLLen
            it "retain sum of original lists" $ property prop_rotateLSum
            it "wrap around when rotated a full lap" $ property prop_rotateLWraps
        context "rotateR" $ do
            it "retain length of original lists" $ property prop_rotateRLen
            it "retain sum of original lists" $ property prop_rotateRSum
            it "wrap around when rotated a full lap" $ property prop_rotateRWraps

-- Properties

prop_euclidLen (Positive k) (NonNegative n) = length (euclid k n) == k

prop_euclidSum (Positive k) (NonNegative n) = sum (euclid k n) == n

prop_superEuclidLen (NonNegative n) = forAll (nonEmptyNonNegativeInts n) $ \ks -> length (superEuclid ks n) == sum ks

prop_superEuclidSum (NonNegative n) = forAll (nonEmptyNonNegativeInts n) $ \ks -> sum (superEuclid ks n) == n

prop_nthNothingForNegativeIndices (Negative i) xs = isNothing $ nth i xs where types = xs :: [Int]

prop_nthNothingForOutOfBoundsIndices (Positive n) xs = isNothing $ nth (length xs + n) xs where types = xs :: [Int]

prop_nthReturnsValueForIndicesInBounds (Positive n) xs = if null xs then isNothing f else isJust f
  where
    f     = nth (n `mod` length xs) xs
    types = xs :: [Int]

prop_rotateLLen (NonNegative n) = forAll (nonNegativeInts 100) $ \xs -> length (rotateL n xs) == length xs

prop_rotateRLen (NonNegative n) = forAll (nonNegativeInts 100) $ \xs -> length (rotateR n xs) == length xs

prop_rotateLSum (NonNegative n) = forAll (nonNegativeInts 100) $ \xs -> sum (rotateL n xs) == sum xs

prop_rotateRSum (NonNegative n) = forAll (nonNegativeInts 100) $ \xs -> sum (rotateR n xs) == sum xs

prop_rotateLWraps = forAll (nonNegativeInts 100) $ \xs -> rotateL (length xs) xs == xs

prop_rotateRWraps = forAll (nonNegativeInts 100) $ \xs -> rotateR (length xs) xs == xs

-- Generators

{-| @nonEmptyNonNegativeInts@ returns a generator of non-empty lists of integers with values between 1 and given value `n`.
-}
nonEmptyNonNegativeInts :: Int -> Gen [Int]
nonEmptyNonNegativeInts n = oneof [return <$> choose (1, n), (:) <$> choose (1, n) <*> nonEmptyNonNegativeInts n]

{-| @nonNegativeInts@ returns a generator of lists of integers with values between 1 and given value `n`.
-}
nonNegativeInts :: Int -> Gen [Int]
nonNegativeInts n = oneof [return [], (:) <$> choose (1, n) <*> nonNegativeInts n]
