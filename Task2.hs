module Task2 where

import Data.List as L
import Data.Char as C
import Task2Message as Task2Message
import Data.Either

--source, match
isStringFound :: String -> String -> Bool
isStringFound x y = 
    if length y == matchCounter x y
    then True
    else False

matchCounter :: String -> String -> Int
matchCounter (x:xs) (y:ys) =
    if x == y
    then 1 + matchCounter xs ys
    else 0
matchCounter _ _ = 0

getHeader :: String -> String
getHeader ('p':xs) = 
    if (isStringFound xs "rev") == True
    then "prev"
    else ""
getHeader ('l':xs) = 
    if (isStringFound xs "ast") == True
    then "last"
    else ""
getHeader('0':xs) = 
    if (isStringFound xs "\":{") == True
    then "0"
    else ""
getHeader _ = ""


isXo :: String -> Bool
isXo "X" = True
isXo "O" = True
isXo _ = False

createJSonXO:: Int -> String  -> (Int, String, (String, JsonLikeValue) )
createJSonXO a (x:xs) = 
    let
        xoVal =  take 1 (drop 3 xs) --dropina " ir :
        isCorrect =  isStringFound xs ("\":\"" ++ xoVal ++ "\",") || isStringFound xs ("\":\"" ++ xoVal ++ "\"}")
    in
    if isXo xoVal == True && isCorrect == True
    then (a+7, drop 5 xs, (x:[], JLString xoVal))  
    else (a, "! not XO or bad formatting at " ++ show (a+5) ++ xoVal, ("", JLString ""))
createJSonCoordinate :: Int -> String  -> (Int, String, (String, JsonLikeValue) )
createJSonCoordinate a (x:xs) = 
    let
        number = L.takeWhile C.isDigit (drop 2 xs) --dropina " ir :
        isCorrect = isStringFound xs ("\":" ++ number ++ ",") || isStringFound xs ("\":" ++ (take 1 number) ++ "}")
        --isNumberCheck = isItNumber number 
        argument = drop 4 xs
    in
    if isCorrect == True && number /= ""
    then (a+5, argument, (x:[], JLInt (read (number))))  
    else (a, "! not an int or bad formatting " ++ show a, ("", JLString ""))

createJMap :: Int -> String -> (Int, String, (String, JsonLikeValue) )
createJMap a x =  
    let
        header = getHeader x
        whatsLeftAfterHeader =  drop (length header + 2) x--dropina " ir :
        objectSize = bracketCounter whatsLeftAfterHeader 
        object = take objectSize whatsLeftAfterHeader
        argument = drop objectSize whatsLeftAfterHeader
        (str, array) = fillJmapStart a object
        newIndex = length header + 6 + a +objectSize-- { " x ":
    in
        case str of
        ('!':xs) -> (a, str, ("", JLString ""))
        otherwise -> if header == ""
                     then (a, "! header is not written properly at " ++ show (a+length header+5), (str, JLString ""))
                     else (newIndex, argument, (header, JLMap (array)))

createJsonObject :: Int -> String -> (Int, String, (String, JsonLikeValue) ) 
createJsonObject a ('\"':'\"':xs) = (a, "! double comma at " ++ show a  ++ "!", ("", JLString ""))
createJsonObject a (',':'{':xs) = (a, "! extra comma at " ++ show a  ++ "!", ("", JLString ""))
createJsonObject a x@('v':xs) = createJSonXO a x 
createJsonObject a x@('x':xs) = createJSonCoordinate a x
createJsonObject a x@('y':xs) = createJSonCoordinate a x
createJsonObject a x@('0':xs) = createJMap a x 
createJsonObject a x@('l':xs) = createJMap a x
createJsonObject a x@('p':xs) = createJMap a x
createJsonObject a (x:xs) = createJsonObject (a+1) xs
createJsonObject a _ = (a, "",("", JLString ""))


fillJmapStart :: Int -> String -> (String, [(String, JsonLikeValue)])
fillJmapStart a b = fillJmap a b ("", []) 

fillJmap :: Int -> String -> (String, [(String, JsonLikeValue)]) -> (String, [(String, JsonLikeValue)])
fillJmap _ "" (x, []) = (x, [])
fillJmap _ "" b = b
fillJmap a x (err, arr) =
    let 
    (position, whatsLeft, (header, object)) = createJsonObject a x  
    in
    case whatsLeft  of
        ('!':xs) -> (whatsLeft, arr)
        _ ->   if header == ""
                then (err, arr)
                else fillJmap position whatsLeft (err, arr ++ (header, object):[])     

parse :: Int -> String -> Either String JsonLikeValue
parse a b = 
    let 
    (err, arr) = fillJmapStart 0 b
    in
    case err of
    "" -> Right $ JLMap (arr)
    a -> Left a 

bracketCounter :: String -> Int
bracketCounter x = bracketCounter' 0 0 x 0
bracketCounter' :: Int -> Int -> String -> Int -> Int -- { kiekis, } kiekis, string, current index,  
bracketCounter' a b ('{':xs) c = 
    if a > b || b == 0
    then bracketCounter' (a+1) b xs (c+1) 
    else c
bracketCounter' a b ('}':xs) c = 
    if a > b || b == 0
    then bracketCounter' a (b+1) xs (c+1) 
    else c
bracketCounter' a b (x:xs) c = 
    if a > b || b == 0
    then bracketCounter' a b xs (c+1) 
    else c
bracketCounter' _ _ "" a = a

getElementAt :: [[a]] -> (Int, Int) -> a
getElementAt matrix (x, y) = (matrix !! y) !! x

jLMapas ::  (String, [(String, JsonLikeValue)])
jLMapas = fillJmapStart 0 message'

getData :: (String, [(String, JsonLikeValue)]) -> [(String, JsonLikeValue)]
getData a = arr
    where 
    (str, arr) = a       


convert :: Int -> JsonLikeValue -> Either InvalidState To  
convert a b = 
    let 
      (err, sign, unconvertedList) = accumulate ("", 'l', []) (unboxMap b)   
    in
    case err of
        "Duplicate" -> Left Duplicates
        "Order" -> Left Order
        _ -> Right (transformList 0 unconvertedList [])  

mainn =  convert size $ JLMap [("prev", JLMap [("last", JLMap [("0", JLMap [("v", JLString "X"), ("y", JLInt 1), ("x", JLInt 0)])]), ("prev", JLMap [("last", JLMap [("0", JLMap [("v", JLString "X"), ("y", JLInt 0), ("x", JLInt 1)])])])]), ("last", JLMap [("0", JLMap [("v", JLString "X"), ("y", JLInt 0), ("x", JLInt 0)])])]

accumulate :: (String, Char, [(Int, Int, Char)]) -> [(String, JsonLikeValue)] -> (String, Char, [(Int, Int, Char)]) 
accumulate a [] = a
accumulate (err, sign, arr) source = 
    let
        (firstStr, firstObj) = source !! 0
        
        (x, y, v) =
            if firstStr == "last"
            then getConvertedMove firstObj
            else 
                let 
                    (secondStr, secondObj) =  source !! 1
                in
                getConvertedMove secondObj

        whatToSendNext = --boxed
            case length source of
                2 -> 
                    if firstStr == "last" 
                    then 
                        let
                            (secondStr, secondObj) =  source !! 1
                        in
                        secondObj
                    else firstObj
                _ -> JLMap []

        err2 = if isInList arr (x, y, v) 
            then "Duplicate"
            else 
                if v==sign
                then "Order"
                else  err  
    in
        case err2 of
            "" -> 
                if length source == 1
                then (err, sign, insertMove arr (x, y, v))
                else accumulate (err, v,  insertMove arr  (x, y, v)) (unboxMap whatToSendNext) --insertMove
            _ -> (err2, 'a', [])

testArr :: [(Int, Char)] 
testArr = []

testList :: [(Int, Int, Char)]
testList = [(0, 1, 'X'), (1, 1, 'X'), (2, 2, 'X')]

isInList :: [(Int, Int, Char)] -> (Int, Int, Char) -> Bool
isInList a b = isInList' a b
isInList' ::  [(Int, Int, Char)] -> (Int, Int, Char) -> Bool
isInList' (head:xs) (x, y, v) = 
    let 
        (x1, y1, v1) = head
    in 
    if x1 == x && y1 == y
    then True
    else isInList xs (x, y, v)
isInList' _ _ = False

transformList :: Int -> [(Int, Int, Char)] -> [[(Int, Char)]]  -> [[(Int, Char)]] 
transformList 15 _ b = b 
transformList x c b = 
    let
       array =  getWhereYArr x c []
    in
    if array == []
    then transformList (x+1) c b    
    else transformList (x+1) c (b ++ array:[])   

getWhereYArr :: Int -> [(Int, Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
getWhereYArr z [] b = b   
getWhereYArr a ((x, y, v):xs) b =
     if y == a 
     then getWhereYArr a xs (b++ (x, v):[])
     else getWhereYArr a xs b


getPositionSplit :: [(Int, Int, Char)] -> (Int, Int, Char) -> Int
getPositionSplit a b = getPositionSplit' (a++(99, 99, 'z'):[]) b 0
getPositionSplit' ((x, y, v):xs) (x1, y1, v1) counter =
    if y1 < y 
    then counter 
    else 
        if y==y1 && x1 <= x 
        then counter
        else getPositionSplit' xs (x1, y1, v1) (counter+1) 


insertMove arr triple = insertAt (getPositionSplit arr triple) triple arr  

insertAt :: Int -> a-> [a] -> [a] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

unboxMap :: JsonLikeValue -> [(String, JsonLikeValue)]
unboxMap (JLMap a) = a

unboxString :: JsonLikeValue -> String
unboxString (JLString a) = a

unboxInt :: JsonLikeValue -> Int
unboxInt (JLInt a) = a

getConvertedMove :: JsonLikeValue -> (Int, Int, Char)
getConvertedMove a =
    let
        (zero, valueMap) = (unboxMap a) !! 0 
        unboxedValueArray = unboxMap valueMap
        y = lookForY unboxedValueArray
        x = lookForX unboxedValueArray
        v = lookForV unboxedValueArray
    in
    (x, y, v)

testMap = JLMap [("0", JLMap [("x", JLInt 1), ("y", JLInt 0), ("v", JLString "O")])]


lookForX :: [(String, JsonLikeValue)] -> Int 
lookForX ((str, jsonVal):xs) = 
    if str == "x"
    then unboxInt jsonVal
    else lookForX xs
lookForX x = unboxInt num 
     where (str, num) = x !! 0

lookForY :: [(String, JsonLikeValue)] -> Int 
lookForY ((str, jsonVal):xs) = 
    if str == "y"
    then unboxInt jsonVal
    else lookForY xs
lookForY x = unboxInt num 
     where (str, num) = x !! 0
lookForV :: [(String, JsonLikeValue)] -> Char 
lookForV ((str, jsonVal):xs) = 
    if str == "v"
    then unboxString jsonVal !! 0
    else lookForV xs
lookForV x = unboxString val !! 0
     where (str, val) = x !! 0