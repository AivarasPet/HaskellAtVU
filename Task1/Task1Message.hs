module Task1Message
where

-- ┌       ┐
-- │ O O   │
-- │   X O │
-- │ X X X │
-- └       ┘
-- seed: 915545711498141579
-- encoding: JsonMap
-- from: COO
-- to: ARR

size :: Int
size = 3

message :: String
message = "{\"0\":{\"x\":\"0\",\"y\":\"0\",\"v\":\"O\"},\"1\":{\"x\":\"1\",\"y\":\"0\",\"v\":\"O\"},\"2\":{\"x\":\"1\",\"y\":\"1\",\"v\":\"X\"},\"3\":{\"x\":\"2\",\"y\":\"1\",\"v\":\"O\"},\"4\":{\"x\":\"0\",\"y\":\"2\",\"v\":\"X\"},\"5\":{\"x\":\"1\",\"y\":\"2\",\"v\":\"X\"},\"6\":{\"x\":\"2\",\"y\":\"2\",\"v\":\"X\"}}"

type From = [(Int, Int, Char)]
type To = ([Int], [Int], [Char])

expectedFrom :: From
expectedFrom = [(0, 0, 'O'), (1, 0, 'O'), (1, 1, 'X'), (2, 1, 'O'), (0, 2, 'X'), (1, 2, 'X'), (2, 2, 'X')]

expectedTo :: To
expectedTo = ([0, 1, 1, 2, 0, 1, 2], [0, 0, 1, 1, 2, 2, 2], ['O', 'O', 'X', 'O', 'X', 'X', 'X'])
