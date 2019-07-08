{-# LANGUAGE ScopedTypeVariables #-}
module SorteeSpec where

import Data.List ((!!))
import Data.Traversable (forM)
import Test.QuickCheck

import Test.Hspec (describe, it, Spec)
import qualified Test.HUnit as HUnit

import Data.String.Sortee (chars, between, Sortee(..))

instance Arbitrary Sortee where
    arbitrary = sized $ \n-> do
                size <- choose (1, n)
                string <- forM [0..size] $ \_-> choose (0, length chars - 1)
                pure $ Sortee $ (chars !!) <$> string

assertResult :: Result -> IO ()
assertResult = HUnit.assertBool "" . isSuccess

spec :: Spec
spec = do
    describe "Sortee" $ do

        it "generates value in between" $ do
            result <- quickCheckResult $ \(a, b)->
                let (smaller :: Sortee, larger :: Sortee) = if a < b then (a, b) else (b, a)
                    answer = between (Just a) (Just b)
                in if larger == (Sortee "0") || smaller == larger || isInvalid smaller larger
                       then True
                       else maybe False (\x-> x > smaller && x < larger) answer
            assertResult result

        it "can swaps arguments" $ do
            result <- quickCheckResult $ \(a, b)->
                let answer1 = between (Just a) (Just b)
                    answer2 = between (Just b) (Just a)
                in answer1 == answer2
            assertResult result

        it "must generate values that can be further processed" $ do
            result <- quickCheckResult $ \(a, b)->
                let (smaller :: Sortee, larger :: Sortee) = if a < b then (a, b) else (b, a)
                    answer = between (Just a) (Just b)
                in case answer of
                    Nothing -> True
                    Just ans ->
                        let withSmall = case between (Just smaller) (Just ans) of
                                            Just x -> x > smaller && x < ans
                                            Nothing -> False
                            withLarge = case between (Just ans) (Just larger) of
                                            Just x -> x > ans && x < larger
                                            Nothing -> False
                        in withSmall && withLarge
            assertResult result
    where isInvalid (Sortee (a:_)) (Sortee (b: '0': _)) | a == b = True
          isInvalid _ _ = False
