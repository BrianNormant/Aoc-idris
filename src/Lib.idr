module Lib

import Data.List
import Data.List.Extra
import Data.Vect

genInd : Nat -> Nat -> List (Nat, Nat)
genInd s l =
  let
    helper : Nat -> Nat -> Nat -> List (Nat, Nat)
    helper len sz cur =
      if (cur+sz > len) then Nil
                        else List.(++) [(cur, sz)] (helper len sz (cur+1))
  in
    if s > l then Nil
    else helper l s 0

genSubStr : Nat -> String -> List (String, Nat)
genSubStr _ "" = []
genSubStr 0 _  = []
genSubStr n s  = map (\p@(i,j) => (substr i j s, i)) (genInd n (length s))

||| return the position of each needle in the haystack
||| @haystack the string to search
||| @needle the substring to search for
export
strstr : String -> String -> List Nat
strstr haystack needle =
  map (\p@(_, i) => i)
  $ filter (\p@(s, _) => s == needle )
  $ genSubStr (length needle) haystack

||| Replace repl in haystack
||| following the position in pos
||| haystack the string to modify
||| repl the replacement
||| pos the position to replace
||| size the size of the needle to replace
export
strrepl : String -> List Nat -> Nat -> String -> String
strrepl "" _  _ hay = hay
strrepl _  _  _ ""  = ""
strrepl _ Nil _ hay = hay
strrepl repl (pos::xs) size haystack =
  let
  haystack = unpack haystack
  (b, a) = splitAt pos haystack
  a = drop size a
  a = (unpack repl) ++ a
  in strrepl repl xs size (pack (b ++ a))

||| Generate a range
export
range : Nat -> List Integer
range Z = []
range (S n) = (cast (n+1)) :: range (n)

export
range' : Integer -> Integer -> List Integer
range' n m = map (+n-1) $ range (cast(m-n+1))

liftA2 : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftA2 f (Just a) (Just b) = Just (f a b)
liftA2 _ _ _ = Nothing

||| Combine 2 Maybe
export
combineMaybe : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe f ma mb = liftA2 f ma mb <|> ma <|> mb

||| Create a tuple out of 2 maybe
export
pairMaybe : Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe ma mb = pure (,) <*> ma <*> mb

||| zip with index (0-indexed)
export
zipWithIndex : List a -> List (Nat, a)
zipWithIndex = mapi (,)

||| try to index a List, if the index is out of bound return Nothing
export
indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (x :: _) = Just x
indexMaybe (S i) (_::xs) = indexMaybe i xs
indexMaybe _ [] = Nothing

||| try to substract from a nat
export
safePred : Nat -> Maybe Nat
safePred Z = Nothing
safePred (S n) = Just n

||| self-explatory, create the all the permutation of a b
export
permutation : List a -> List b -> List (a,b)
-- permutation xx yy = join $ map (\x => map (\y => (x,y) ) yy) xx
permutation xx yy = concatMap (\x => map (\y => (x,y) ) yy) xx

export
unsafeIndex : Nat -> List a -> a
unsafeIndex n xs =
  let vect : Vect (length xs) a
      vect = fromList xs
      idx : Fin (length xs)
      idx = believe_me n
  in index idx vect

||| apply a function if a condition is true
export
replaceIf : (a -> Bool) -> (a -> a) -> List a -> List a
replaceIf f g (x::xs) = (if f x then g x else x) :: replaceIf f g xs
replaceIf _ _ [] = []



||| map a function if a condition over the index is true
export
replaceIfIndex : (Nat -> Bool) -> (a -> a) -> List a -> List a
replaceIfIndex f g xs = map snd
  $ replaceIf (f . fst) (\(i,v) => (i, g v)) (mapi (,) xs)
