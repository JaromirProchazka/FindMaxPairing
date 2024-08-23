import Parovani
import Debug.Trace (trace)

-- TEST
main = do
  putStrLn "RUN TESTS"
  putStrLn (assert (bfsPath g "a" "e") "(2,[(\"c\",2,0,0),(\"e\",3,0,0)])" "BFS - Najdi nenasicenou cestu")
  putStrLn ""
  putStrLn (assert (fordfalk diamant "a" "c") "[(\"a\",[(\"b1\",1,1),(\"b2\",1,1)]),(\"b1\",[(\"b2\",0,1),(\"c\",1,1)]),(\"b2\",[(\"c\",1,1)]),(\"c\",[])]" "Ford-Falk - klasicky diamant")
  putStrLn (assert (fordfalk square 1 8) "[(1,[(2,6,6),(3,5,6)]),(2,[(4,4,4),(5,2,2)]),(3,[(5,5,9),(2,0,5)]),(4,[(8,7,7)]),(5,[(4,3,8),(8,4,4)]),(8,[(2,0,1),(3,0,4)])]" "Ford-Falk - sit s spetnymi hranami ze stoku")
  putStrLn ""
  putStrLn (assert (parovani chess_basic) "[(\"1\",\"h\"),(\"2\",\"g\"),(\"3\",\"f\"),(\"4\",\"e\"),(\"5\",\"d\"),(\"6\",\"c\"),(\"7\",\"b\"),(\"8\",\"a\")]" "Parovani - veze na normalni sachovnici")
  --   a b c d e f g h
-- 1 . . . . . . . T
-- 2 . . . . . . T .
-- 3 . . . . . T . .
-- 4 . . . . T . . .
-- 5 . . . T . . . .
-- 6 . . T . . . . .
-- 7 . T . . . . . .
-- 8 T . . . . . . .
  putStrLn (assert (parovani chess_no_diagonal) "[(\"1\",\"g\"),(\"2\",\"e\"),(\"3\",\"f\"),(\"4\",\"c\"),(\"5\",\"d\"),(\"6\",\"a\"),(\"7\",\"b\")]" "Parovani - veze na sachovnici bez diagonaly")
--   a b c d e f g
-- 1 . . . . . . T
-- 2 . . . . T   .
-- 3 . . . .   T .
-- 4 . . T   . . .
-- 5 . .   T . . .
-- 6 T   . . . . .
-- 7   T . . . . .
  putStrLn (assert (parovani chess_not_square) "[(\"1\",\"f\"),(\"2\",\"e\"),(\"3\",\"d\"),(\"4\",\"c\"),(\"5\",\"b\"),(\"6\",\"a\")]" "Parovani - veze na sachovnici ne ctverec")
--   a b c d e f g h
-- 1 . . . . . T . .
-- 2 . . . . T . . .
-- 3 . . . T . . . .
-- 4 . . T . . . . .
-- 5 . T . . . . . .
-- 6 T . . . . . . .
  putStrLn (assert (parovani chess_only_black) "[(\"1\",\"g\"),(\"2\",\"h\"),(\"3\",\"e\"),(\"4\",\"f\"),(\"5\",\"c\"),(\"6\",\"d\"),(\"7\",\"a\"),(\"8\",\"b\")]" "Parovani - veze na sachovnici jen cerna pole")
--   a b c d e f g h
-- 1 .   .   .   T  
-- 2   .   .   .   T
-- 3 .   .   T   .  
-- 4   .   .   T   .
-- 5 .   T   .   .  
-- 6   .   T   .   .
-- 7 T   .   .   .  
-- 8   T   .   .   .
  putStrLn (assert (parovani chess_only_black_with_holes) "[(\"1\",\"c\"),(\"2\",\"f\"),(\"3\",\"g\"),(\"4\",\"h\"),(\"5\",\"e\"),(\"6\",\"b\"),(\"7\",\"a\"),(\"8\",\"d\")]" "Parovani - veze na sachovnici jen cerna pole a s dirami")
--   a b c d e f g h
-- 1 .   T   .   .  
-- 2   .   .   T   .
-- 3 .       .   T  
-- 4   .   .       T
-- 5 .       T   .  
-- 6   T   .   .   .
-- 7 T   .   .   .  
-- 8       T   .   .


assert :: (Show x) => x -> String -> String-> String
assert actual expected ""
  | (actual_str == expected) = "Test Prosel"
  | otherwise = ">> Test NEProsel! " ++ actual_str ++ " mel byt " ++ expected
  where
    actual_str = show actual
assert actual expected label
  | (actual_str == expected) = "Test " ++ label ++  " Prosel"
  | otherwise = ">> Test " ++ label ++ " NEProsel! " ++ actual_str ++ " mel byt " ++ expected
  where
    actual_str = show actual

diamant :: Graf String
diamant =
  [ ("a", [("b1", 0, 1), ("b2", 0, 1)]),
    ("b1", [("b2", 0, 1), ("c", 0, 1)]),
    ("b2", [("c", 0, 1)]),
    ("c", [])
  ]

diamant_expected :: String
diamant_expected = "[(\"a\",[(\"b1\",1,1),(\"b2\",1,1)]),(\"b1\",[(\"b2\",0,1),(\"c\",1,1)]),(\"b2\",[(\"c\",1,1)]),(\"c\",[])]"

g :: Rezervy String
g =
  [ ("a", [("b", 2, 0, 0), ("c", 2, 0, 0), ("f", 5, 0, 0)]),
    ("b", [("d", 3, 0, 0)]),
    ("c", [("b", 2, 0, 0), ("e", 3, 0, 0)]),
    ("d", [("c", 4, 0, 0)]),
    ("e", [("d", 2, 0, 0)]),
    ("f", [("c", 1, 0, 0), ("e", 2, 0, 0)])
  ]

square :: Graf Int
square = 
  [
    (1, [(2, 0, 6), (3, 0, 6)]),
    (2, [(4, 0, 4), (5, 0, 2)]),
    (3, [(5, 0, 9), (2, 0, 5)]),
    (4, [(8, 0, 7)]),
    (5, [(4, 0, 8), (8, 0, 4)]),
    (8, [(2, 0, 1), (3, 0, 4)])
  ]

chess_basic :: [(String, [String])]
chess_basic =
  [ ("1", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("2", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("3", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("4", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("5", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("6", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("7", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("8", ["a", "b", "c", "d", "e", "f", "g", "h"])
  ]

chess_no_diagonal :: [(String, [String])]
chess_no_diagonal =
  [ ("1", ["a", "b", "c", "d", "e", "f", "g"]),
    ("2", ["a", "b", "c", "d", "e",      "g"]),
    ("3", ["a", "b", "c", "d",      "f", "g"]),
    ("4", ["a", "b", "c",      "e", "f", "g"]),
    ("5", ["a", "b",      "d", "e", "f", "g"]),
    ("6", ["a",      "c", "d", "e", "f", "g"]),
    ("7", [     "b", "c", "d", "e", "f", "g"])
  ]

chess_not_square :: [(String, [String])]
chess_not_square =
  [ ("1", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("2", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("3", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("4", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("5", ["a", "b", "c", "d", "e", "f", "g", "h"]),
    ("6", ["a", "b", "c", "d", "e", "f", "g", "h"])
  ]

chess_only_black :: [(String, [String])]
chess_only_black =
  [ ("1", ["a",      "c",      "e",      "g"     ]),
    ("2", [     "b",      "d",      "f",      "h"]),
    ("3", ["a",      "c",      "e",      "g"     ]),
    ("4", [     "b",      "d",      "f",      "h"]),
    ("5", ["a",      "c",      "e",      "g"     ]),
    ("6", [     "b",      "d",      "f",      "h"]),
    ("7", ["a",      "c",      "e",      "g"     ]),
    ("8", [     "b",      "d",      "f",      "h"])
  ]

chess_only_black_with_holes :: [(String, [String])]
chess_only_black_with_holes =
  [ ("1", ["a",      "c",      "e",      "g"     ]),
    ("2", [     "b",      "d",      "f",      "h"]),
    ("3", ["a",                "e",      "g"     ]),
    ("4", [     "b",      "d",                "h"]),
    ("5", ["a",                "e",      "g"     ]),
    ("6", [     "b",      "d",      "f",      "h"]),
    ("7", ["a",      "c",      "e",      "g"     ]),
    ("8", [               "d",      "f",      "h"])
  ]

--   a b c d e f g h
-- 1 . . . . . . . .
-- 2 . . . . . . . .
-- 3 . . . . . . . .
-- 4 . . . . . . . .
-- 5 . . . . . . . .
-- 6 . . . . . . . .
-- 7 . . . . . . . .
-- 8 . . . . . . . .