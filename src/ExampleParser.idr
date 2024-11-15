module ExampleParser
--- This is a example of using the parser module to parse a regular text
--- Here are the important steps:
--- 1. Define the tokens you want to match as Enum/data
--- 2. implements the Eq and TokenKind inferface for it
--- 3. map each of them to their lex, ex Number -> digits
---    need to use a TokenMap
--- 4. Define the grammar of the text.
---    It's best to go from the smallest unit up to the whole sentence
---    to add 2 tokens, use the do notation, ex:
---      do n <- match Number
---         match Space
---         match Color
---    the `<-` can be used to get the result of a submatch
---    finally compose the tokens with pure function:
---         pure (n)
---    here are some regex operators equivalence:
---    | : <|>
---    + : many
---    * : some
---    ? : optional / ( option can give a default value )
---    {n} : count (exactly n)
---    {n,m} : count (between n m)
---    {n,} : count (atLeast n)
---    {,n} : count (atMost n)
---    (?://) : just ignore the result
---    (//) : use the `<-` to capture the result
---
---    here are some regex tokens equivalence: ( This use the lexer)
---    with the TokenMap from step 3
---    a : is 'a'
---    [abc] : oneOf "abc"
---    [^abc] : non (oneOf "abc")
---    a case insensitive : like
---    [abc] case insensitive : ---
---    [a-e] : range 'a' 'e'
---    a+ : some `lexer`
---    \d  : digit
---    \d+ : digits
---    [01]: binDigit
---    [0-9A-Fa-f]: hexDigit
---    [0-8] : octDigit
---    [A-Za-z] : alpha
---    [a-z] : lower
---    [A-Z] : upper
---    [A-Za-z0-9] : alphaNum
---     : newline
---    a(.*)a : surround a `lexer (.*)`
---    . : any
---
---   This is not a regex copy, It can do both more and less:
---     It can capture multiple sub group, ex: ( (\d+)+)
---     This regex would only capture the last digit
---     But Data.Parser can do it easily
---     Yet this : (\d+)\1 can't be implemented with Text.Parser
---     Because it doesn't allow `backtracing`


import Text.Parser
import Text.Lexer
import Data.List1

str : String
str = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

data Color = CRed | CGreen | CBlue

-- lexing
data GameKind = Start
  | Number
  | Red
  | Green
  | Blue
  | SepIgn
  | SepCol
  | SepCom
  | SepSem

Eq GameKind where
  Number == Number = True
  Red    == Red    = True
  Green  == Green  = True
  Blue   == Blue   = True
  SepIgn == SepIgn = True
  SepCol == SepCol = True
  SepCom == SepCom = True
  SepSem == SepSem = True
  Start  == Start  = True
  _     == _      = False

TokenKind GameKind where
  TokType Number = Nat
  TokType Red    = Color
  TokType Green  = Color
  TokType Blue   = Color
  TokType SepIgn = ()
  TokType SepCol = ()
  TokType SepCom = ()
  TokType SepSem = ()
  TokType Start  = ()

  tokValue Number x = cast x
  tokValue Red    _ = CRed
  tokValue Green  _ = CGreen
  tokValue Blue   _ = CBlue
  tokValue SepIgn _ = ()
  tokValue SepCol _ = ()
  tokValue SepCom _ = ()
  tokValue SepSem _ = ()
  tokValue Start  _ = ()

GameToken : Type
GameToken = Token GameKind

tmap : TokenMap GameToken
tmap = [
  (exact "Game",  Tok Start),
  (digits,        Tok Number),
  (exact "red",   Tok Red),
  (exact "green", Tok Green),
  (exact "blue",  Tok Blue),
  (is ':',        Tok SepCol),
  (is ',',        Tok SepCom),
  (is ';',        Tok SepSem),
  (spaces,        Tok SepIgn)
  ]


||| match and capture /\s+?(\d+)\s+(green|blue|red)/
matchCube : Grammar state GameToken True (Nat, Color)
matchCube = do option () (match SepIgn)
               n <- match Number
               match SepIgn
               c <- match Red <|> match Green <|> match Blue
               pure (n, c)

||| match multiple cubes separated by /,/
matchStep : Grammar state GameToken True (List1 (Nat, Color))
matchStep = sepBy1 (match SepCom) matchCube

||| match multiple steps separated by /;/
matchGame : Grammar state GameToken True (List1 (List1 (Nat, Color)))
matchGame = sepBy1 (match SepSem) matchStep

defaultList : List1 (List1 (Nat, Color))
defaultList = ?dl

||| match the whole line
grammar : Grammar state GameToken True (Nat, List1 (List1 (Nat, Color)))
grammar = do match Start
             match SepIgn
             game <- match Number
             match SepCol
             g <- matchGame
             pure (game, g )

test : String -> (Nat, List (List (Nat, Color)))
test s = let toks = fst $ lex tmap s in
         case parse grammar toks of
              Right ((n, l), _) => (n, forget $ map forget l )
              Left _ => (0, [])
