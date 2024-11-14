-- module Template
module Day3

import Lib

import Data.String
import Data.List
import Data.List.Extra
import Data.Vect

import System.File

FILENAME : String
FILENAME = "./data/day3-input.txt"


record TypeNum where
     constructor MkTypeNum
     num  : Nat
     line : Nat
     sCol : Nat
     len  : Nat

implementation Show TypeNum where
     show (MkTypeNum n l s e) =
          "MkTypeNum(" ++ (show n) ++ " "
          ++ (show l) ++ " " ++ (show s)
          ++ " " ++ (show e) ++ ")"

findNums' : Nat -> List Char -> Nat -> List TypeNum
findNums' _ [] _ = []
findNums' line s col =
     let
     -- find a number if possible
     num = takeWhile isDigit s
     in if num == [] then
                  findNums' line (drop 1 s) (col+1)
     else MkTypeNum (maybe 0 id (parsePositive $ pack num)) line col (length num)
     :: findNums' line (drop (length num) s) (col + (length num))

||| Find number in line
||| Pair them with their position in the line
findNums : Nat -> String -> List TypeNum
findNums line s = findNums' line (unpack s) 0

--- Grab the symbols arround those position
charsArround : TypeNum -> List String -> List Char
charsArround (MkTypeNum _ i s e) l =
     let
     a = case i of -- List Maybe Char
              0 => Nothing
              S i => map (substr s e) $ indexMaybe i l;
     b = map (substr s e) $ indexMaybe (i+1) l;
     a = maybe [] ((map Just) . unpack) a;
     b = maybe [] ((map Just) . unpack) b;

     ll = map unpack $ indexMaybe i l;
     ri = case s of -- Maybe Char
              0 => Nothing
              S s => join $ map (indexMaybe s) ll;
     le = join $ map (indexMaybe (s+e)) ll;
     ar = case (i, s) of
               (0, _) => Nothing
               (_, 0) => Nothing
               (S i, S s) => join $ map (indexMaybe s) $ map unpack $ indexMaybe i l;
     al = case i of
               0 => Nothing
               S i => join $ map (indexMaybe (s+e)) $ map unpack $ indexMaybe i l;
     br = case s of
               0 => Nothing
               S s => join $ map (indexMaybe s) $ map unpack $ indexMaybe (i+1) l;
     bl = join $ map (indexMaybe (s+e)) $ map unpack $ indexMaybe (i+1) l;
     in catMaybes (a ++ b ++ [ar, al, br, bl, ri, le])

||| generate the direct neighbors coord of a coordinates.
||| offset by (x,y) and limited by (maxX, maxY) (exclusive)
||| does not include the center
directNeighbors : (Nat, Nat) -> (Nat, Nat) -> List (Nat, Nat)
directNeighbors (maxX, maxY) (x,y) =
     let
     xx : List Nat;
     yy : List Nat;
     m : List (Nat, Nat);
     xx = catMaybes [safePred x, Just x, Just (S x)];
     yy = catMaybes [safePred y, Just y, Just (S y)];
     m =  filter (\(x,y) => x < maxX && y < maxY)
          $ filter (/= (x,y))
          $ permutation xx yy;
     in m

testCharsArround : IO ()
testCharsArround =
     let
     s = [
          "abd",
          "e1f",
          "gh*j"
          ];
     tn = MkTypeNum 18 1 1 1;
     in do
          printLn $ charsArround tn s;

||| Get the number at the given position in a text
getNumHere : List TypeNum -> (Nat, Nat) -> Maybe (Nat, Nat, Nat)
getNumHere nums (line, col) = head'
     $ map (\(MkTypeNum num l c _) => (l, c, num))
     $ filter (\(MkTypeNum _ l s e) => l == line && col >= s && col < s+e ) nums

sol1 : String -> Nat
sol1 s =
     let ll = lines s
     in sum
          $ map (.num . fst)
          $ filter (any (/= '.') . snd) -- If any symbol != '.' keep the number
          $ map (\t => (t, apply (flip charsArround ll) t)) -- List (TypeNum, List Char)
          $ join
          $ mapi findNums ll -- List TypeNum

sol2 : String -> ?ty -- List (Nat, List (Nat, Char))
--- List (Nat, List (Nat, Char))
--- List (Nat, Nat)
sol2 s = let
     ll = lines s;
     nums = join $ mapi findNums ll
     tmp = id
       $ sum
       $ map (foldl mult 1)
       $ map (map (\(_,_,n) => n))
       $ filter ((== 2) . length)
       $ map nub
       $ map (mapMaybe (getNumHere nums))
       $ map (directNeighbors (length $ unsafeIndex 0 ll, length ll))
       $ join
       $ map (\(line, l) => map (\(col, _) => (line, col)) l)
       $ filter (\p@(_,l) => not $ isNil l) -- (not . isNil . snd)
       $ map (\p@(i,c) => (i, filter ((== '*') . snd) c))
       $ mapi (\i,v => (i, mapi (,) v))
       $ map unpack $ ll;
     in tmp

ex1 : String
ex1 = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

ex2 : String
ex2 = ex1

export
run1 : IO()
-- run1 = printLn $ show $ sol1 ex1
run1 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol1 line
               Left _ => putStrLn "Error reading file"
export
run2 : IO()
-- run2 = printLn $ show $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
