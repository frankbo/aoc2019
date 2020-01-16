module DayTwoSpec where

import           Test.Hspec
import           DayTwo                         ( intCodeProgram )

spec :: Spec
spec = describe "DayTwo" $ do
        it "should finish when the array starts with 99" $ do
                (intCodeProgram [99, 1, 2, 3]) `shouldBe` [99, 1, 2 ,3]
        it "should add" $ do
                (intCodeProgram [1, 0, 0, 0, 99]) `shouldBe` [2, 0, 0, 0, 99]
        it "should multiply" $ do
                (intCodeProgram [2, 3, 0, 3, 99]) `shouldBe` [2, 3, 0, 6, 99]
        it "should add and the stop" $ do
                (intCodeProgram [1, 0, 0, 0, 99, 1, 2, 3]) `shouldBe` [2, 0, 0, 0,99 ,1 ,2 ,3]
        it "should first add then muliply" $ do
                (intCodeProgram [1, 0, 0, 0, 2, 0, 0, 3, 99]) `shouldBe` [2, 0, 0, 4, 2, 0, 0, 3, 99]
	it "should work with the example" $ do
                (intCodeProgram [2,4,4,5,99,0]) `shouldBe` [2,4,4,5,99,9801]
	it "should work with the example two" $ do
                (intCodeProgram [1,1,1,4,99,5,6,0,99]) `shouldBe` [30,1,1,4,2,5,6,0,99]
