import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Euclid
import Control.Monad

main :: IO ()
main = hspec $ do
    describe "euclid" $ do
        it "should return k distances" $ property prop_euclidLen
        it "distances should sum up to n" $ property prop_euclidSum
    describe "superEuclid" $ do
        it "should return # of distances equal to sum of ks" $ property prop_superEuclidLen
        it "distances should sum up to n" $ property prop_superEuclidSum

prop_euclidLen (Positive k) (NonNegative n) = length (euclid k n) == k

prop_euclidSum (Positive k) (NonNegative n) = sum (euclid k n) == n

prop_superEuclidLen (NonNegative n) = forAll (nonNegativeInts n) $ \ks -> length (superEuclid ks n) == sum ks

prop_superEuclidSum (NonNegative n) = forAll (nonNegativeInts n) $ \ks -> sum (superEuclid ks n) == n

{-| @nonNegativeInts@ returns a generator of non-empty lists of integers with values between 1 and given value `n`.
-}
nonNegativeInts :: Int -> Gen [Int]
nonNegativeInts n = oneof [return <$> choose (1, n), (:) <$> choose (1, n) <*> nonNegativeInts n]
