module DayOneSpec where

import           DayOne                         ( fuelForModules )
import           Test.Hspec

spec :: Spec
spec = describe "DayOne" $ do
        it "should calculate corretct fuel for one modules" $ do
                (fuelForModules [100756]) `shouldBe` 50346
        it "should calculate corretct fuel for two modules" $ do
                (fuelForModules [100756, 1969]) `shouldBe` 51312
