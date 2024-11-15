module Day4

import Lib

import Data.String
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Stream
import Text.Lexer
import Text.Parser
import Debug.Trace

import System.File

FILENAME : String
FILENAME = "./data/day4-input.txt"

data StratchKind = Start
                 | SepSpc
                 | Number
                 | SepCol
                 | SepVer

TokenKind StratchKind where
     TokType Number = Nat
     TokType SepSpc = ()
     TokType SepCol = ()
     TokType SepVer = ()
     TokType Start  = ()

     tokValue Number x = cast x
     tokValue SepSpc _ = ()
     tokValue SepCol _ = ()
     tokValue SepVer _ = ()
     tokValue Start  _ = ()

Eq StratchKind where
     Number == Number = True
     SepSpc == SepSpc = True
     SepCol == SepCol = True
     SepVer == SepVer = True
     Start  == Start  = True
     _      == _      = False

StratchToken : Type
StratchToken = Token StratchKind

tmap : TokenMap StratchToken
tmap = [
     (digits, Tok Number),
     (spaces, Tok SepSpc),
     (is ':', Tok SepCol),
     (is '|', Tok SepVer),
     (exact "Card", Tok Start)
     ]

numsGrammar : Grammar state StratchToken True (List1 Nat)
numsGrammar = do option () (match SepSpc)
                 l <- sepBy1 (match SepSpc) (match Number)
                 option () (match SepSpc)
                 pure l

scratchGrammar : Grammar state StratchToken True (Nat, List1 Nat, List1 Nat)
scratchGrammar = do match Start
                    match SepSpc
                    n <- match Number
                    match SepCol
                    nums1 <- numsGrammar
                    match SepVer
                    nums2 <- numsGrammar
                    pure (n, nums1, nums2)

stratchParse : String -> (Nat, List Nat, List Nat)
stratchParse str = let toks = fst $ lex tmap str in
                       case parse scratchGrammar toks of
                            Right ((n, l, r), _) => (n, forget l, forget r)
                            Left _ => (0, [], [])

sol1 : String -> ?ry1
sol1 = sum
       . ( map ((pow 2) . (\x => x - 1) . cast ) )
       . ( filter (/= 0) )
       . ( map length )
       . ( map (\(_, w,h) => filter (\a => isJust $ find (== a) w) h) )
       . ( map stratchParse )
       . lines



record Strach where
     constructor MkStrach
     i : Nat

||| 4 1 -> [2,3,4,5]
fn : Nat -> Nat -> List Nat
fn n i = take n $ Stream.iterate (+1) (i+1)

fn2 : List (Nat, List Nat) -> List Nat
fn2 [] = []
fn2 (x@(_, d)::xs) =
     let dup = map (\dd => maybe (1,[]) id $ find ( (== dd) . fst) xs) d
         xs = xs ++ dup
         -- xs = groupBy (\(a,_),(b,_) => a == b) xs
         -- [ [(i,[]), ...], ...] => [(i, []), ...]
         -- xs = map (
         --      foldl (\acc,(i,l) => (i, (snd acc) ++ l)) (0, Prelude.Nil)
         --      ) xs
     in  (fst x) :: ( fn2 xs )

-- [1,3] => [2,3,4] => r[2]+1, r[3]+1, r[4]+1
-- [2,2] => [3,4]   => r[3]+2, r[3]+2
-- [3,1] => [4]     => r[4]+3
-- [4,0] => []      => pass
-- `r` is the result: a list that store the ammount of each card
fn3' : List (Nat, Nat) -> List Nat -> List (Nat, Nat)
fn3' [] r = mapi (\i,v => (i,v)) r
fn3' (p::xs) r =
     let
     (i,n) = p
     f = maybe 1 id $ indexMaybe i r
     r = replaceIfIndex (\i' => i' > i && i' <= i+n) (+f) r
     in fn3' xs r


fn3 : List (Nat, Nat) -> List (Nat, Nat)
fn3 [] = []
fn3 l  = let r = take ((length l) + 1) $ Stream.repeat 1
         in fn3' l r

sol2 : String -> Nat
sol2 = sum
         . (map snd)
         -- List (Nat,Nat) : List of each card and it's count
         -- List (Nat, Nat) : List of each card and how many new card it should create
         -- . fn2 -- No point in representing the cards.
         -- . ( map (\(n, l) => (n, ( fn l n ))))
         . drop 1
         . fn3
         . ( map (\(n, l) => (n, length l)) )
         . ( map (\(n, w, h) => (n, filter (\a => isJust $ find (== a) w ) h) ) )
         . ( map stratchParse )
         . lines

ex1 : String
ex1 = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
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
