import Parovani

-- TEST
diamant :: Graf String
diamant =
  [ ("a", [("b1", 0, 1), ("b2", 0, 1)]),
    ("b1", [("b2", 0, 1)]),
    ("b1", [("c", 0, 1)]),
    ("b2", [("c", 0, 1)]),
    ("c", [])
  ]

g :: Graf String
g =
  [ ("a", [("b", 0, 2), ("c", 0, 2), ("f", 0, 5)]),
    ("b", [("d", 0, 3)]),
    ("c", [("b", 0, 2), ("e", 0, 3)]),
    ("d", [("c", 0, 4)]),
    ("e", [("d", 0, 2)]),
    ("f", [("c", 0, 1), ("e", 0, 2)])
  ]

bip :: [(String, [String])]
bip =
  [ ("1", ["a", "c", "d", "e", "f", "g"]),
    ("2", ["a", "b", "c", "d", "e", "g"]),
    ("3", ["a", "c", "d", "f", "g"]),
    ("4", ["a", "b", "c", "d", "e", "f", "g"]),
    ("5", ["a", "c", "d", "e", "f", "g"]),
    ("6", ["a", "b", "c", "d", "f"]),
    ("7", ["a"])
  ]

main = do
  -- print "A - B"
  -- print (fordfalk g "a" "b")
  print "Neohrožující se věže na děravé šachovnici:"
  print (fordfalk diamant "a" "c")
  print (parovani bip)