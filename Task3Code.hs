module Task3 where
import Data.List as L
import System.IO
import System.Environment 
import System.Exit
import Data.Char as C

type To = [(Int, Int, Char)]
data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] deriving (Show, Eq)
data InvalidState = Order | Duplicates deriving (Show, Eq)


main :: IO ()
main = do
    args <- getArgs
    messageReceived <- getLine
    let playerChar = args!!0!!0 
    if messageReceived == "*" 
    then do
      putStr "{\"last\":{\"0\":{\"data\":{\"0\":0,\"1\":2,\"2\":\"X\"}}}}"
      hPrint stderr $ "My move: (0, 2, 'X')"
      exitWith ExitSuccess
    else do
      let parsed = parse 0 messageReceived
      case parsed of
        Left parseResp -> do hPrint stderr $ parseResp
                             exitWith (ExitFailure 100)
        Right parseResp -> case converted of
            Left Duplicates -> do exitWith (ExitFailure 101)
                                  hPrint stderr $ "Duplicates!"
            Left Order -> do exitWith (ExitFailure 101)
                             hPrint stderr $ "Order error!"
            Right convertResp -> do 
              case (gameSituation tableInCharListOfLists 'X') of
                2 -> do let bestMove = findBestMove tableInCharListOfLists playerChar
                        let latestTable = updateMatrix tableInCharListOfLists bestMove
                        hPrint stderr $ latestTable!!0
                        hPrint stderr $ latestTable!!1
                        hPrint stderr $ latestTable!!2
                        hPrint stderr $ getBestMoveStr bestMove
                        putStr $ appendStr bestMove messageReceived
                        let latestResult = gameSituation latestTable playerChar
                        case latestResult of 
                          1 -> do hPrint stderr "You won!"
                                  exitWith (ExitFailure 10)
                          0 -> do hPrint stderr "It is a draw!"
                                  exitWith (ExitFailure 12)
                          _ -> exitWith ExitSuccess
                _ -> do hPrint stderr $ "Game is finished"
                        exitWith (ExitFailure 20)
              where tableInCharListOfLists = prepareList convertResp
            where converted = convert' 0 parseResp

getBestMoveStr (x, y, v) = "My move: (" ++ show x ++ ", " ++ show y ++ ", " ++ v:[] ++ " )"
oppositeXO :: Char -> Char
oppositeXO x = 
    if x == 'X'
    then 'O'
    else 'X'  


-- will be called with a board and either x or o
-- and it will tell whether that tile wins
doesPlayerWin :: [[Char]] -> Char -> Bool
doesPlayerWin b c = 
   any (\row -> (all (\col ->  (b!!row!!col)  == c ) [0..2])) [0..2]
   || any (\col -> (all (\row ->  (b!!row!!col)  == c ) [0..2])) [0..2]
   ||  (b!!0!!0) ==  c &&  (b!!1!!1) == c &&   (b!!2!!2) == c
   ||  (b!!0!!2) ==  c &&  (b!!1!!1) == c &&   (b!!2!!0) == c


prepareList :: [(Int, Int, Char)] -> [[Char]]
prepareList x =  prepareList' x ["   ", "   ", "   "]
prepareList' :: [(Int, Int, Char)] -> [[Char]] -> [[Char]]
prepareList' ((x, y, v):xs) arr = prepareList' xs (updateMatrix arr  (x, y, v))
prepareList' x y =  y

updateMatrix :: [[Char]] ->(Int, Int, Char) -> [[Char]]
updateMatrix m (x,y,v) =
  take y m ++
  [take x (m !! y) ++ [v] ++ drop (x + 1) (m !! y)] ++
  drop (y + 1) m


isGameStillOn ::  [[Char]] -> Char -> Bool
isGameStillOn arr x  = contains (arr!!0) x || contains (arr!!1) x || contains (arr!!2) x
contains :: [Char] -> Char -> Bool
contains[] _ = False
contains (x:xs) n 
  | x == n = True
  | otherwise = contains xs n

startSearchingEmpty :: [[Char]] -> [(Int, Int, Char)]
startSearchingEmpty x =  filter (\(y, z, c) -> c == ' ') $ startSearchingEmpty' x 0 
startSearchingEmpty' :: [[Char]] -> Int -> [( Int, Int, Char)] 
startSearchingEmpty' (x:xs) y = zip3 [0,1,2] (y:y:y:[])  x ++ startSearchingEmpty' xs (y+1)
startSearchingEmpty' _ y = []

 --board -> available moves -> playerChar -> best move
findBestMove :: [[Char]] -> Char -> (Int, Int, Char)
findBestMove board playerChar = 
  let
    availableMoves = startSearchingEmpty board
    results = map (\(xx, yy, vv) -> (findBestMove' (updateMatrix board (xx, yy, playerChar)) playerChar (oppositeXO playerChar))) availableMoves -- returns: [0, 1, -1, ...]
    (finalX, finalY, uselessV) = availableMoves !! (findMaxIndex results (-100) 0 0)
  in
  (finalX, finalY, playerChar)
-- board -> available moves -> PlayerChar -> currentChar -> win/loss/tie
findBestMove' :: [[Char]] ->  Char -> Char -> Int -- (1 - win, 0 tie, -1 - loss) returnin *-1 ez
findBestMove' board playerChar currentChar = --current Char kieno ejimas, jei pralosia grazina -1
  if gameSituation board playerChar /= 2 --if game not over
  then gameSituation board playerChar 
  else 
    let
      availableMoves = startSearchingEmpty board
      results = map (\(xx, yy, vv) -> (findBestMove' (updateMatrix board (xx, yy, currentChar)) playerChar (oppositeXO currentChar))) availableMoves -- returns: [0, 1, -1, ...]
    in
    if playerChar == currentChar
    then (maximum results)   
    else (minimum results) 
 -- pirmas entry i findBestMove' yra kai cChar != pChar, so reik minimumo
findMaxIndex :: [Int] -> Int -> Int -> Int -> Int
findMaxIndex (x:xs) maxi currIndex answer = 
  if x <= maxi 
  then findMaxIndex xs maxi (currIndex+1) answer
  else findMaxIndex xs x (currIndex+1) currIndex 
findMaxIndex _ _ _ c = c

gameSituation :: [[Char]] -> Char -> Int 
gameSituation board playerChar
    | doesPlayerWin board $ oppositeXO playerChar = - 1
    | doesPlayerWin board playerChar = 1
    | not(isGameStillOn board ' ') = 0
    | otherwise = 2

appendStr :: (Int, Int, Char) -> String -> String
appendStr (x, y, v) str = "{\"last\":{\"0\":{\"data\":{\"0\":" ++ show x ++ ",\"1\":" ++ show y ++ ",\"2\":\"" ++ v:[] ++ "\"}}},\"prev\":" ++ str ++ "}"

--https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
--https://stackoverflow.com/questions/29991307/check-if-a-player-wins-in-a-tic-tac-toe-game

--TASK2:

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
getHeader ('d':xs) =
    if(isStringFound xs "ata") == True
    then "data"
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
createJSonCoordinate :: Int -> String  -> (Int, String, (String, JsonLikeValue) ) --createJSonCoordinate 0 "0\":2," -> (5,"",("0",JLInt 2))
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
        whatsLeftAfterHeader =  
            if header == "last"
            then drop (length header + 5) x--dropina " ir :
            else drop (length header + 2) x
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
createJsonObject a ('\"':'\"':xs) = (a, "! double comma at " ++ show a  ++ "!", ("", JLString "")) --createJsonObject 0 "data\":{\"0\":2,\"1\":2,\"2\":\"O\"}" fails
createJsonObject a (',':'{':xs) = (a, "! extra comma at " ++ show a  ++ "!", ("", JLString ""))
createJsonObject a x@('1':xs) = createJSonCoordinate a x
createJsonObject a x@('0':xs) = createJSonCoordinate a x
createJsonObject a x@('2':xs) = createJSonXO a x 
createJsonObject a x@('d':xs) = createJMap a x 
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
        _ -> Right unconvertedList   

convert' :: Int -> JsonLikeValue -> Either InvalidState [(Int, Int, Char)]  
convert' a b = 
    let 
      (err, sign, unconvertedList) = accumulate ("", 'l', []) (unboxMap b)   
    in
    case err of
        "Duplicate" -> Left Duplicates
        "Order" -> Left Order
        _ -> Right  unconvertedList   

connector :: Either String JsonLikeValue -> JsonLikeValue
connector a = case a of
    Left a -> JLString ""
    Right a -> a  


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

unboxEMap :: Either String JsonLikeValue -> [(String, JsonLikeValue)]
unboxEMap x = case x of
    Right (JLMap a) -> a
    b -> []

unboxString :: JsonLikeValue -> String
unboxString (JLString a) = a

unboxInt :: JsonLikeValue -> Int
unboxInt (JLInt a) = a

getConvertedMove :: JsonLikeValue -> (Int, Int, Char) --takes ("data", JLMAP [] )
getConvertedMove a =
    let
        (zero, valueMap) = (unboxMap a) !! 0 -- zero == "data"
        unboxedValueArray = unboxMap valueMap
        y = lookForY unboxedValueArray
        x = lookForX unboxedValueArray
        v = lookForV unboxedValueArray
    in
    (x, y, v)

lookForX :: [(String, JsonLikeValue)] -> Int 
lookForX ((str, jsonVal):xs) = 
    if str == "0"
    then unboxInt jsonVal
    else lookForX xs
lookForX x = unboxInt num 
     where (str, num) = x !! 0

lookForY :: [(String, JsonLikeValue)] -> Int 
lookForY ((str, jsonVal):xs) = 
    if str == "1"
    then unboxInt jsonVal
    else lookForY xs
lookForY x = unboxInt num 
     where (str, num) = x !! 0
lookForV :: [(String, JsonLikeValue)] -> Char 
lookForV ((str, jsonVal):xs) = 
    if str == "2"
    then unboxString jsonVal !! 0
    else lookForV xs
lookForV x = unboxString val !! 0
     where (str, val) = x !! 0

