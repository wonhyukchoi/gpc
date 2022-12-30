module TestUtils ( buildExpectations ) where

import Test.Hspec ( Spec )

buildExpectations :: (a -> Spec) -> [a] -> Spec
buildExpectations mkExpectation =
  foldr (\input acc -> acc >> mkExpectation input) (return ())