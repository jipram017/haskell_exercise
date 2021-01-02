module Main where

-- Convert an integer to a list of digits
toDigits::Integer -> [Integer]
toDigits n
   | n <=0 = []
   | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
 
toDigitsTest::Bool 
toDigitsTest = and
 [ 
     [1] == toDigits 1,
     [1,2,3,4] == toDigits 1234,
     [] == toDigits 0,
     [] == toDigits (-17)
 ]

-- Convert an integer to a list of digits in reverse
toDigitsRev::Integer -> [Integer]
toDigitsRev n
    | n <=0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigitsRevTest::Bool
toDigitsRevTest = and
  [
      [1] == toDigitsRev 1,
      [4,3,2,1] == toDigitsRev 1234,
      [] == toDigitsRev 0,
      [] == toDigitsRev (-17)
  ]

-- Double every other number beginning from the right
doubleEveryOther::[Integer] -> [Integer]
doubleEveryOther = reverse . zipWith(*) (cycle [1,2]) . reverse

doubleEveryOtherTest::Bool
doubleEveryOtherTest = and
  [
      [5] == doubleEveryOther [5],
      [10,5] == doubleEveryOther [5,5],
      [1,4,0] == doubleEveryOther [1,2,0],
      [10,5,12,5] == doubleEveryOther [5,5,6,5]
  ]

-- Return the sum of all digits of each number of the list
sumDigits::[Integer] -> Integer
sumDigits = sum . concat . map toDigits

sumDigitsTest::Bool
sumDigitsTest = and
  [
      5 == sumDigits[23],
      10 == sumDigits [5,5],
      22 == sumDigits [16,7,12,5]
  ]

-- Indicates whether or not a credit card number is valid
validate::Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

validateTest::Bool
validateTest = and
  [
      True == validate 4012888888881881,
      False == validate 4012888888881882
  ]

main = do
    putStrLn "Running ..."
    print toDigitsTest
    print toDigitsRevTest
    print doubleEveryOtherTest 
    print sumDigitsTest
    print validateTest
    