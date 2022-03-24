module Test where

import Test.HUnit
import QueenFormulaGen


{- Tests for checkCut -}

testCheckCut1 :: Test
testCheckCut1 = TestCase (assertEqual "Same coloumn" True (checkCut (4,3) (5,3)))

testCheckCut2 :: Test
testCheckCut2 = TestCase (assertEqual "Same diagonal" True (checkCut (1,1) (2,2)))

testCheckCut3 :: Test
testCheckCut3 = TestCase (assertEqual "Same row" True (checkCut (3,4) (3,5)))

{- Tests for fromPairToSingle -}

testFromPairToSingle1 :: Test
testFromPairToSingle1 = TestCase (assertEqual "" 1 $ fromPairToSingle 5 (0, 0))

testFromPairToSingle2 :: Test
testFromPairToSingle2 = TestCase (assertEqual "" 1 $ fromPairToSingle 6 (0, 0))

testFromPairToSingle3 :: Test
testFromPairToSingle3 = TestCase (assertEqual "" 2 $ fromPairToSingle 5 (0, 1))

testFromPairToSingle4 :: Test
testFromPairToSingle4 = TestCase (assertEqual "" 12 $ fromPairToSingle 5 (2, 1))

{- Tests for fromSinglePair -}

testFromSingleToPair1 :: Test
testFromSingleToPair1 = TestCase (assertEqual "" (0,0) $ fromSingleToPair 5 1)

testFromSingleToPair2 :: Test
testFromSingleToPair2 = TestCase (assertEqual "" (0,0) $ fromSingleToPair 6 1)

testFromSingleToPair3 :: Test
testFromSingleToPair3 = TestCase (assertEqual "" (0,1) $ fromSingleToPair 5 2)

testFromSingleToPair4 :: Test
testFromSingleToPair4 = TestCase (assertEqual "" (2,1) $ fromSingleToPair 5 12)

doubleTest :: Int -> Test
doubleTest n = TestCase (assertEqual "" 1 $ fromPairToSingle n (fromSingleToPair n 1))

{- Tests for getCNFRows -}
testGetCNFRows1 :: Test
testGetCNFRows1 = TestCase (assertEqual "" [[1]] (getCNFRows 1))

testGetCNFRows2 :: Test
testGetCNFRows2 = TestCase (assertEqual "" [[1,2], [3,4]] (getCNFRows 2))

testGetCNFRows3 :: Test
testGetCNFRows3 = TestCase (assertEqual "" [[1, 2, 3], [4, 5, 6], [7,8,9]] (getCNFRows 3))

{- Tests getCells -}
testGetCells0 :: Test
testGetCells0 = TestCase (assertEqual "" [] $ getCells 0)

testGetCells1 :: Test
testGetCells1 = TestCase (assertEqual "" [(0, 0)] $ getCells 1)

testGetCells2 :: Test
testGetCells2 = TestCase (assertEqual "" [(0,0),(0,1),(1,0),(1,1)] $ getCells 2)

{- Tests getCNFTwoCells -}

testGetCNFTwoCells0 :: Test 
testGetCNFTwoCells0 = TestCase (assertEqual "" [] $ getCNFTwoCells 0)

testGetCNFTwoCells1 :: Test 
testGetCNFTwoCells1 = TestCase (assertEqual "" [] $ getCNFTwoCells 1)

testGetCNFTwoCells2 :: Test 
testGetCNFTwoCells2 = TestCase (assertEqual "" [[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4]] $ getCNFTwoCells 2)

tests :: Test
tests = TestList [
    {- Testing possible cases for checkCut function -}
    TestLabel "Same coloumn"  testCheckCut1,
    TestLabel "Same diagonal" testCheckCut2,
    TestLabel "Same row"      testCheckCut3,

    {- Testing possible cases for fromPairToSingle function -}
    TestLabel "(0,0) for size 5x5" testFromPairToSingle1,
    TestLabel "(0,0) for size 6x6" testFromPairToSingle2,
    TestLabel "(0,1) for size 5x5" testFromPairToSingle3,
    TestLabel "(2,1) for size 5x5" testFromPairToSingle4,

    {- Testing possible cases for fromSingleToPair function -}
    TestLabel "1 for size 5x5"  testFromSingleToPair1,
    TestLabel "1 for size 6x6"  testFromSingleToPair2,
    TestLabel "2 for size 5x5"  testFromSingleToPair3,
    TestLabel "12 for size 5x5" testFromSingleToPair4,

    {- Testing both previous function -}
    TestLabel "double test for 0"   $ doubleTest 0,
    TestLabel "double test for 1"   $ doubleTest 1,
    TestLabel "double test for 2"   $ doubleTest 2,
    TestLabel "double test for 100" $ doubleTest 100,

    {- Testing small cases for getCNFRows function -}
    TestLabel "CNF for rows for size 1x1" testGetCNFRows1,
    TestLabel "CNF for rows for size 2x2" testGetCNFRows2,
    TestLabel "CNF for rows for size 3x3" testGetCNFRows3,

    {- Testing small cases for getCells -}
    TestLabel "All possible cells for size 0" testGetCells0,
    TestLabel "All possible cells for size 1" testGetCells1,
    TestLabel "All possible cells for size 2" testGetCells2,

    {- Testing small cases for getCNFTwoCells -}
    TestLabel "CNF for each two not beating each other size 0" testGetCNFTwoCells0,
    TestLabel "CNF for each two not beating each other size 1" testGetCNFTwoCells1,
    TestLabel "CNF for each two not beating each other size 2" testGetCNFTwoCells2

    ]

main :: IO ()
main = runTestTTAndExit tests
