module Test.Main where

import Prelude
import Data.Foldable (sum)
import Data.Iterator (Iterator, empty, fromArray, next, range, singleton, toArray)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ]
    $ describe "Iterator" do
        describe "Properties" do
          it "has empty iterators" $ count empty `shouldEqual` 0
          it "has singleton iterators size 1" $ quickCheck \(x :: Int) -> (count $ singleton x) === 1
          it "has singleton iterators of the right value" $ quickCheck \(x :: Char) -> (fst <<< next $ singleton x) === x
          it "converts from/to array" $ quickCheck \(arr :: Array String) -> toArray (fromArray arr) === arr
          it "generates cartesian products from Monad"
            $ 1000
                `shouldEqual`
                  count do
                    let
                      step = range ((+) 1)
                    a <- step 0 10
                    b <- step 0 10
                    c <- step 0 10
                    pure (a + b * c)

count :: forall a. Iterator a -> Int
count iter = sum $ const 1 <$> iter
