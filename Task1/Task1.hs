module Task1 where

import Data.List as L
import Data.Char as C
import Task1Message as Task1Message

type Fromm = (Int, Int, Char)

---- converts from message to "To"
main :: To
main = convert size (parse size message)


----- parses message to "From"

parse :: Int -> String -> From --same as From
parse _ x = result
    where (result, y) = accumulatorFrom (drop 2 x ) []

accumulatorFrom :: String -> [Fromm] -> ([Fromm], String) 
accumulatorFrom ('{':'\"':xs) acc = 
        accumulatorFrom whatsLeft (acc ++ triplet:[] ) 
    where
        (whatsLeft, triplet) = getATriplet xs
accumulatorFrom ('}':'}':_) x = (x, "") 
accumulatorFrom (_:xs) acc = accumulatorFrom xs acc
accumulatorFrom _ _ = ([], "")

getATriplet :: String -> (String, Fromm)
getATriplet str =
    let 
        (i1, left1) = (scanForChar str)
        (i2, left2) = (scanForChar left1)
        (c3, left3) = scanForChar left2
    in
     (left3, ( read (i1:[]), read (i2:[]), c3))


scanForChar :: String -> (Char, String)
scanForChar ('X':xs) = ('X', xs) 
scanForChar ('O':xs) = ('O', xs)
scanForChar (x:xs) =
    let 
        isLegit = C.isDigit x
    in 
        case isLegit of
            True -> (x, xs)
            otherwise -> scanForChar xs


------- to convert ---------

convert :: Int -> From -> To
convert _ x = ( (getArrOfSingleTripleEl 1 x), ( (getArrOfSingleTripleEl 2 x)), ((getArrOfThirdTripleEl x)))--(xArr, yArr, charArr)
 
getArrOfSingleTripleEl :: Int -> From -> [Int]
getArrOfSingleTripleEl  1 (x:xs) =  (tripleGetFirst x) : (getArrOfSingleTripleEl 1 xs)
getArrOfSingleTripleEl  2 (x:xs) =  (tripleGetSec x) : (getArrOfSingleTripleEl 2 xs)
getArrOfSingleTripleEl _  _ = []

getArrOfThirdTripleEl (x:xs) = (tripleGetThird x) : (getArrOfThirdTripleEl  xs )
getArrOfThirdTripleEl _ = []

tripleGetFirst (x, _, _) = x
tripleGetSec (_, y, _) = y
tripleGetThird (_, _, z) = z

