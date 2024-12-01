-- module Template
module Day5

import Lib

import Data.String
import Data.List
import Debug.Trace
import Text.Lexer
import Text.Parser

import System.File

import Generics.Derive
import Derive.Eq

%language ElabReflection

FILENAME : String
FILENAME = "./data/day5-input.txt"

------------------------------[ Parsing ]---------------------------------------
data AlmanacKind = Word
                 | Number
                 | SepLin
                 | SepSpc

%runElab derive "AlmanacKind" [Derive.Eq.Eq]

TokenKind AlmanacKind where
  TokType Word   = String
  TokType Number = Integer
  TokType SepLin = ()
  TokType SepSpc = ()

  tokValue Word   x = cast x
  tokValue Number x = cast x
  tokValue SepLin _ = ()
  tokValue SepSpc _ = ()

AlmanacToken : Type
AlmanacToken = Token AlmanacKind

tmap : TokenMap AlmanacToken
tmap = [
  (some $ non (digit <|> space <|> newline), Tok Word),
  (digits, Tok Number),
  (newline, Tok SepLin),
  (spaces, Tok SepSpc)
  ]

||| match seeds: 12 13 ...\n
matchHeader : Grammar state AlmanacToken True (List1 Integer)
matchHeader = do _ <- match Word
                 match SepSpc
                 n <- sepBy1 (match SepSpc) (match Number)
                 match SepLin
                 pure n

||| match thing-to-other map:\n
matchBegin : Grammar state AlmanacToken True ()
matchBegin = do _ <- match Word
                match SepSpc
                _ <- match Word
                match SepLin
                pure ()

||| match to from len | ex: 12 32 54
matchMapper : Grammar state AlmanacToken True (Integer, Integer, Integer)
matchMapper = do to   <- match Number
                 match SepSpc
                 from <- match Number
                 match SepSpc
                 len  <- match Number
                 pure (to, from, len)

matchAlmanac : Grammar state AlmanacToken True (List1 Integer, List1 (List1 (Integer, Integer, Integer)) )
matchAlmanac = do l1 <- matchHeader
                  match SepLin
                  l2 <- sepBy1 (match SepLin) matchMappers
                  pure (l1, l2)
               where
                 matchMappers : Grammar state AlmanacToken True ( List1 (Integer, Integer, Integer) )
                 matchMappers = do matchBegin
                                   l <- sepBy1 (match SepLin) matchMapper
                                   option () ( match SepLin )
                                   pure l

record Mapper where
  constructor MkMapper
  from : Integer
  to   : Integer
  len  : Integer

parseAlmanac : String -> (List Integer, List (List Mapper))
parseAlmanac str = let toks = fst $ lex tmap str in
                         case parse matchAlmanac toks of
                              Right ((l1, l2), _) => (
                                forget l1,
                                l2 |> forget |> (map (map (\(t, f, l) => MkMapper f t l) ||> forget))
                                )
                              Left err => ([], [])


convert : List Mapper -> List Integer -> List Integer
convert maps = map (tryconv maps)
                 where
                   total
                   tryconv : List Mapper -> Integer -> Integer
                   tryconv [] i = i
                   tryconv (m::ms) i = if (i >= m.from && i <= m.from + m.len)
                                          then i + m.to - m.from
                                          else tryconv ms i

testConv : List Integer
testConv = let maps = [ (MkMapper 98 50 2),
                        (MkMapper 50 52 48) ]
            in convert maps [79, 14, 55, 13]


||| Convert a range of seeds instead of a singular seed
||| 000   0
|||  ¦ -> 50
||| 100   100
convert' : List Mapper -> List (Integer, Integer) -> List (Integer, Integer)
convert' mappers = map (convRange mappers) ||> join
where
  convRange : List Mapper -> (Integer, Integer) -> List (Integer, Integer)
  convRange [] i = [i]
  convRange (m::ms) (i, j) = let ma = max i m.from;
                                 mi = min j (m.from + m.len);
                                 rl = i;
                                 rh = j;
                                 offset = m.to - m.from;
                                 in if (ma > mi)
                                       then convRange ms (i, j) -- no feat, try to map with the other mappers
                                       else [(ma + offset, mi + offset)] -- map this section
                                       ++ ( map (convRange ms) ( -- try map the 2 other section if exist with other mappers
                                              (if rl /= ma then [(rl, ma - 1)] else [])
                                              ++ (if rh /= mi then [(mi + 1, rh)] else [])
                                              ) |> join )

sol1 : String -> ?sol1ty
sol1 s = let (seeds, mappers) = parseAlmanac s
          in (foldl (flip convert) seeds mappers)
             -- |> traceVal
             |> foldl min 2000000000



sol2 : String -> ?sol2ty
sol2 s = let (seeds, mappers) = parseAlmanac s
             seeds = seeds
                   |> ( (grouped 2) )
                   |> mapMaybe (\l => case l of
                          [x, y] => Just (x, x+y)
                          _ => Nothing
                          )
          in (foldl (flip convert') seeds mappers)
             |> traceVal
             |> map fst
             |> foldl min 2000000000

ex1 : String
ex1 = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
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

--- let x the time taken to accelerate.
---     t the total time we have
---     a the acceleration constant
---     f(x) the distance realized in the time left
---     f(x) = ax(t-x)
---          = axt - ax²
--- on cherche les valeurs pour lequels f(x) = const.
---    -ax² + axt - const = 0

