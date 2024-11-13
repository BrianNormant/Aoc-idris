module Day2

import Lib
import Data.String
import Data.List
import Data.List1

import System.File

record Game where
  constructor MkGame
  id : Nat
  cube_r : Nat
  cube_g : Nat
  cube_b : Nat

implementation Show Game where
  show (MkGame i r g b) =
    "MkGame(" ++ (show i) ++ ", "
    ++ (show r) ++ ", " ++ (show g) ++ ", "
    ++ (show b) ++ ")"

implementation Eq Game where
  (==) (MkGame i _ _ _) (MkGame j _ _ _) = i == j

||| Combine 2 game to find the maximun of each color
max : Game -> Game -> Game
max (MkGame a r1 g1 b1) (MkGame _ r2 g2 b2) =
  MkGame a (max r1 r2) (max g1 g2) (max b1 b2)


(+>) : Game -> Game -> Game
(+>) (MkGame a r1 g1 b1) (MkGame _ r2 g2 b2) =
  MkGame a (r1+r2) (g1+g2) (b1+b2)

data Color = Red | Blue | Green

maxColor : Color -> Nat
maxColor Red   = 12
maxColor Green = 13
maxColor Blue  = 14

total
parseColor : String -> Maybe Color
parseColor "red" = Just Red
parseColor "blue" = Just Blue
parseColor "green" = Just Green
parseColor _ = Nothing

parseCubes : List String -> List (Color, Nat)
parseCubes [] = []
parseCubes (x :: xs) =
  let
  (int, col) = break (== ' ') $  trim x
  int = parsePositive int
  col = parseColor $ ltrim col
  in case pairMaybe col int of
          Nothing => parseCubes xs
          Just p  => p :: parseCubes xs

||| Create a partial Game
||| With useless id
total
partialGame : (Color, Nat) -> Game
partialGame (Red, c)   = MkGame 0 c 0 0
partialGame (Green, c) = MkGame 0 0 c 0
partialGame (Blue, c)  = MkGame 0 0 0 c

parseGame : String -> List Game
parseGame "" = []
parseGame s =
  let
  (g , r) = break (== ':') s;
  (h , g) = break (== ' ') g;
  n = maybe 0 Prelude.id $ parsePositive g;
  mr = map pack $ tail' $ unpack r;

  fn : Nat -> String -> Game
  fn n =
    (foldl (+>) (MkGame n 0 0 0))
    . (map partialGame) -- List Game
    . parseCubes -- List (Color, Nat)
    . forget -- List String
    . (split (== ','));

  in case mr of
          Just r => map (fn n) $ forget $ split (== ';') r
          Nothing => Nil

isGameValid : Game -> Bool
isGameValid (MkGame _ r g b) =
  r <= (maxColor Red)   &&
  g <= (maxColor Green) &&
  b <= (maxColor Blue)

sol1 : String -> Nat
sol1 = sum
  . ( map (\p@(MkGame i _ _ _) => i))
  . nub
  . join
  . ( filter ( all isGameValid ) )
  . ( map (parseGame) )
  . lines

sol2 : String -> Nat
sol2 = sum
  . ( map (\p@(MkGame _ r g b) => r*g*b))
  . ( map (foldl max (MkGame 0 0 0 0)))
  . ( map (parseGame) )
  . lines

ex1 : String
ex1 = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

export
run1 : IO()
-- run1 = printLn $ show $ sol1 ex1
run1 = do file <- readFile "./data/day2-input.txt"
          case file of
               Right line => printLn (show (sol1 line))
               Left _ => putStrLn "Error reading file"

export
run2 : IO()
-- run2 = printLn $ show $ sol2 ex1
run2 = do file <- readFile "./data/day2-input.txt"
          case file of
               Right line => printLn (show (sol2 line))
               Left _ => putStrLn "Error reading file"
