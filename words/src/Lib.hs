module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid 
    , findword
    , findwords
    , findWordInLine
    , skew
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe (catMaybes)
{- 
This is more or less a synonym for [String].
To neaten up the code and prevent repetition of the [String] type variable.
-}
type Grid = [String] 
{-
function declaration, we want this function to take input and give output.
The outputGrid function takes in one parameter grid 
and it helps to instigate the formatGrid function that adds a new line 
character after each string.
-}
outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

{-
type declaration for formatGrid, this function is aimed at looking through the list (grid) and 
then the each line(also a string) and eventually returns a String.
The formatGrid function adds a new line(/n) character after each grid line.

-}
formatGrid :: Grid -> String 
formatGrid  =  unlines -- this adds a new line character(/n) after each string.
{-
    the findword function takes two parameters:grid and word. A new variable called lines
    is defined within the findword function. Lines contains the actual grid cocatenated with the
    grid in reverse order. Note that the in-built function "map" helps to apply the other
    in-built function "reverse" to every line in the grid.
    The function named 'found' is multi-faceted. It contains 'or' which returns True iff one or more
    operands are True. The 'map' function helps to apply the findWordInLine function that takes one parameter,
    word( word is more or less each word or string ) 
    to the lines variable (recall, lines contains the actual grid and also the grid in reverse order).
    The 'in' function naturally succeeds the let function in Haskell, and in this situation it helps to include the lines
    expression in the found expression.
    The Maybe String encapsulates an optional value. A value of type Maybe 'a' either contains a value of 
    type a(represented as Just a), or it is empty represented as Nothing.
-}

getLines :: Grid -> [String]
getLines grid =
    let horizontal = grid 
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal  ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)


diagonalize :: Grid -> Grid
diagonalize grid = transpose (skew grid)

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l: skew (map indent ls)
   where indent line = '_': line 

findword:: Grid -> String -> Maybe String
findword grid word = 
    let lines = getLines grid
        found = or  $ map (findWordInLine word) lines  
    in if found then Just word else Nothing


--findwords :: Grid -> [String] -> [Bool]
{-The findwords function takes two parameters, grid and words.
A variable termed 'foundwords' helps to apply the findword function
as defined above on all strings. catMaybes function takes a list 
of Maybes and returns a list of all the Just values.
-}
findwords grid words =
    let foundwords = map (findword grid) words
    in catMaybes foundwords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf --the isInfixOf function takes two lists and returns True iff the first list is contained in the second list.

grid = [  "_ _C_ _ _ _ _ _ _ _R_ _ _"
        , "_ _SI_ _ _ _ _ _ _ _ U_ _"
        , "_ _HASKELL_ _ _ _ _ _ B_"
        , "_ _A_ A_ _ _ _  S_ _ _ Y"
        , "_ _R_   B _ _  C_ _ _ _ "
        , "_ _P_PHP_ _ _ H _ _ _ _ _"
        , "_ _ _ _S _ LREP _ _ _ _ _" 
        , "_ _ _ _I _ _M Y _L_ _"
        , "_ _ _ _L _ E  T O_ _ _"
        , "_ _ _ _ _ _ _ HB_ _ _ _"
        , "_ _ _ _ _ _ _ O_ _ _ _"
        , "_ _ _ _ _ _ _CN_ _ _ _"

        ]
 
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"

            ]


