{-# OPTIONS_GHC -Wall #-}

module HanoiTower where

type Peg = String
type Move = (Peg, Peg)

hanoi::Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start,end)]
hanoi n start end temp =
    let nMinusOne = subtract 1 n
    in hanoi nMinusOne start temp end ++ 
       hanoi 1 start end temp ++
       hanoi nMinusOne temp end start

hanoiTest::Bool
hanoiTest = and
  [
      [("a","c"), ("a","b"),("c","b")] ==  hanoi 2 "a" "b" "c",
      32767 == (length $ hanoi 15 "a" "b" "c")
  ]

main = do
    print hanoiTest