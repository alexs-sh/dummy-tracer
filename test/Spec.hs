import           Test.HUnit

import           Math                           ( square
                                                , minroot
                                                , magnitude
                                                )
import           Vector                         ( unitVector
                                                , Vector(Vector)
                                                , vectorToTuple
                                                )
import           Point                          ( Point )

testSquare :: Test
testSquare = TestCase (assertEqual "check square" 9 (square 3))

testMagnitude :: Test
testMagnitude = TestCase
  (do
    assertEqual "check magnitude.tuple" (sqrt 3) (magnitude (1, 1, 1))
    assertEqual "check magnitude.vector"
                (sqrt 3)
                (magnitude $ vectorToTuple $ Vector 1 1 1)
  )


testUnitVector :: Test
testUnitVector = TestCase
  (assertEqual "check unitVector"
               1.0
               (magnitude $ vectorToTuple $ unitVector $ Vector 13 22 14)
  )

testMinroot :: Test
testMinroot = TestCase
  (do
    assertEqual "check minroot.quad1" (Just (-40.0)) (minroot (-1) (-30) 400)
    assertEqual "check minroot.quad2" Nothing        (minroot 1 1 1)
    assertEqual "check minroot.lin1"  (Just 2.0)     (minroot 0 1 (-2))
    assertEqual "check minroot.lin2"  Nothing        (minroot 0 0 1)
  )

tests :: Test
tests = TestList [testSquare, testMagnitude, testUnitVector, testMinroot]

main :: IO ()
main = runTestTTAndExit tests
