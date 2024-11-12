module Lib

import Data.List

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
