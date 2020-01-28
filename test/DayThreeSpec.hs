module DayThreeSpec where

import           Test.Hspec
import           DayThree                       ( exercise )

spec :: Spec
spec = describe "DayThree" $ do
        it "should match text" $ do
                (exercise `shouldBe` "hello tet")
