module Main where

import Data.List (transpose)

-- Return every n-th element of a list starting at the 1st element
skipEvery::Int -> [a] -> [a]
skipEvery _ [] = []
skipEvery n (x:xs) = x : skipEvery n (drop n xs)

-- Return every n-th element of a list starting at the nth element
every::Int -> [a] -> [a]
every n = skipEvery n . drop n

-- Return a list of lists where n-th list in the output contains every 
-- n-th element from the input list
skips::[a] -> [[a]]
skips xs = [every i xs | i <- [0..n]]
  where n = length xs - 1

skipsTest::Bool
skipsTest = and
  [
      ["ABCD", "BD", "C", "D"] == skips "ABCD",
      ["hello!", "el!", "l!", "l", "o", "!"] == skips "hello!",
      [[True, False], [False]] == skips [True, False]
  ]

-- Return all the local maxima in the input list in order
-- A local maximum of a list is an element of list which is strictly greater than both of elements 
-- immediately before and after it
localMaxima::[Integer] -> [Integer]
localMaxima l@(x:y:z:_)
  | x < y && y > z = y : localMaxima (tail l)
  | otherwise = localMaxima $ tail l

localMaxima _ = []

localMaximaTest::Bool
localMaximaTest = and 
  [
      [] == localMaxima [1,2],
      [9, 6] == localMaxima [2,9,5,6,1],
      [4] == localMaxima [2,3,4,1,5],
      [] == localMaxima [1,2,3,4,5]
  ]

----- PRINTING HISTOGRAM ----
-- Return the number of occurences of an element in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Return a string of length maxF having f *
bar::Int -> Int -> String
bar f maxF = take maxF $ replicate f '*' ++ repeat ' '

-- Take as input a list of integers between 0 and 9 and returns a textual representation
-- of a vertical histogram showing how many of each number were in the input list
histogram::[Integer] -> String
histogram xs = unlines $ rotate bars ++ legend
  where
      legend = ["==========", "0123456789"]
      frequencies = [count i xs | i <- [0..9]]
      maxF = maximum frequencies
      rotate = reverse . transpose
      bars = [bar f maxF | f <- frequencies]

histogramTest::Bool
histogramTest = and
  [
          "   * *    " ++ legend == h1,
    " *        \n *        \n *   *    " ++ legend == h2,
    "    *     \n    *     \n    * *   \n ******  *" ++ legend == h3
  ]
  where
    h1 = histogram [3, 5]
    h2 = histogram [1, 1, 1, 5]
    h3 = histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
    legend = "\n==========\n0123456789\n"

main = do
    print skipsTest
    print localMaximaTest
    print histogramTest