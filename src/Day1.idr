module Day1

import Data.String
import Data.List
import Data.Vect
import System.File
import Lib

input : List String
input = [
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "treb7uchet"
  ]

firstLast : Vect (S len) a -> Vect 2 a
firstLast a = [head a, last a]

firstLast' : {n : Nat} -> (v:Vect n a) -> Maybe (Vect 2 a)
firstLast' { n = 0 } Nil = Nothing
firstLast' { n = 1 } (x :: Nil) = Just [x, x]
firstLast' a@(x::cx) = Just ( firstLast a )

selectDigit : String -> List Char
selectDigit = (filter isDigit ) . unpack

takeFirstLast : List Char -> Maybe(List Char)
takeFirstLast l = map toList (join (map firstLast' (toVect (length l) l) ) )

number : String -> Maybe Integer
number = join . ( map (parseInteger . pack) ) . takeFirstLast . selectDigit

sumMaybe : Maybe Integer -> Maybe Integer -> Maybe Integer
sumMaybe (Just a) (Just b) = Just (a + b)
sumMaybe (Just a) Nothing = Just a
sumMaybe Nothing _ = Nothing


||| find and replace occurrences of digit written as digit
||| in str by rpl ( which should be numerical represention of di)
findRplDigit : String -> String -> String -> String
findRplDigit di rpl str =
  let
  pos = strstr str di
  in strrepl rpl pos (length di) str

digit   : Nat -> String
digit 1 = "one"
digit 2 = "two"
digit 3 = "three"
digit 4 = "four"
digit 5 = "five"
digit 6 = "six"
digit 7 = "seven"
digit 8 = "eight"
digit 9 = "nine"
digit _ = ""

replaceDigits : String -> String
replaceDigits =
  (findRplDigit "one" "1")
  . (findRplDigit "two" "2")
  . (findRplDigit "three" "3")
  . (findRplDigit "four" "4")
  . (findRplDigit "five" "5")
  . (findRplDigit "six" "6")
  . (findRplDigit "seven" "7")
  . (findRplDigit "eight" "8")
  . (findRplDigit "nine" "9")

findDigitsPos : String -> List (Maybe Nat, Nat)
findDigitsPos s =
  map (\i => (head' $ strstr s (digit $ cast i), cast i)) $ range' 1 9


||| replace the first of occurence of the **first**
||| digit in the string
replaceFirstDigit : String -> String
replaceFirstDigit s =
  let
  pos = head' $ dropWhile (\p@(m,_) => not $ isJust m) $ sort $ findDigitsPos s
  in maybe s (\a => a) $ map (\p@(m, i) => case m of
                            Just n => strrepl (show i) [n] (length $ digit i) s
                            Nothing => s
                            ) pos

||| replace the last of occurence of the **last**
||| digit in the string
replaceLastDigit : String -> String
replaceLastDigit s =
  let
  pos = last' $ sort $ findDigitsPos s
  in maybe s (\a => a) $ map (\p@(m, i) => case m of
                            Just n => strrepl (show i) [n] (length $ digit i) s
                            Nothing => s
                            ) pos

||| Find the first number in base 10: 1,2,3,4 ect
||| Return (number, index)
firstNumB10 : String -> Maybe (Nat, Nat)
-- find first occurence of each number
-- sort and select the first one
firstNumB10 "" = Nothing
firstNumB10 s =
  apply (\p => case p of
        Nothing    => Nothing
        Just (m,n) => case m of
                           Nothing => Nothing
                           Just mm => Just (cast n, mm)
              ) $ head'
  $ dropWhile (\p@(m,_) => isNothing m) $ sort
  $ map (\i => ( head' $ sort $ strstr s (cast i), i)) $ range' 1 9

||| Find the first number written in english
||| one, two, three ...
firstNumENG : String -> Maybe (Nat, Nat)
firstNumENG "" = Nothing
firstNumENG s =
  apply (\p => case p of
        Nothing    => Nothing
        Just (m,n) => case m of
                           Nothing => Nothing
                           Just mm => Just (cast n, mm)
              ) $ head'
  $ dropWhile (\p@(m,_) => not $ isJust m) $ sort
  $ map (\i => ( head' $ sort $ strstr s (digit $ cast i), i)) $ range' 1 9

||| Find the last number in base 10: 1,2,3,4 ect
lastNumB10 : String -> Maybe (Nat, Nat)
lastNumB10 "" = Nothing
lastNumB10 s =
  apply (\p => case p of
        Nothing    => Nothing
        Just (m,n) => case m of
                           Nothing => Nothing
                           Just mm => Just (cast n, mm)
              ) $ last'
  $ sort
  $ map (\i => ( last' $ sort $ strstr s (cast i), i)) $ range' 1 9

||| Find the last number written in english
||| one, two, three ...
lastNumENG : String -> Maybe (Nat, Nat)
lastNumENG "" = Nothing
lastNumENG s =
  apply (\p => case p of
        Nothing    => Nothing
        Just (m,n) => case m of
                           Nothing => Nothing
                           Just mm => Just (cast n, mm)
              ) $ last'
  $ sort
  $ map (\i => ( last' $ sort $ strstr s (digit $ cast i), i)) $ range' 1 9

parseLine : String -> Maybe Integer
parseLine s =
  let
  fn : (Nat -> Nat -> Bool) -> (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
  fn f p1@(_,i1) p2@(_,i2) = if f i1 i2 then p1 else p2

  p = map (\p@(n,_) => show n) $ combineMaybe (fn (<)) (firstNumB10 s) (firstNumENG s)
  q = map (\p@(n,_) => show n) $ combineMaybe (fn (>)) (lastNumB10  s) (lastNumENG  s)
  d = map (\p@(x,y) => parseInteger $ x ++ y) $ pairMaybe p q
  in join d

parseLine1 : String -> Maybe Integer
parseLine1 s =
  let
  fn : (Nat -> Nat -> Bool) -> (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
  fn f p1@(_,i1) p2@(_,i2) = if f i1 i2 then p1 else p2

  p = map (\p@(n,_) => show n) $ firstNumB10 s
  q = map (\p@(n,_) => show n) $ lastNumB10  s
  d = map (\p@(x,y) => parseInteger $ x ++ y) $ pairMaybe p q
  in join d

sol1 : String -> Maybe(Integer)
sol1 = (foldl sumMaybe (Just 0))
       . (map parseLine1)
       . lines

export
run1 : IO()
run1 = do file <- readFile "./data/day1-input.txt"
          case file of
               Right lines => printLn (show (sol1 lines))
               Left _ => putStrLn "Error reading file"

sol2 : String -> Maybe Integer
sol2 = (foldl sumMaybe (Just 0))
       . (map parseLine)
       . lines

export
run2 : IO()
-- run2 = printLn $ show $ sol2 ex2
run2 = do file <- readFile "./data/day1-input.txt"
          case file of
               Right line => printLn (show (sol2 line))
               Left _ => putStrLn "Error reading file"

