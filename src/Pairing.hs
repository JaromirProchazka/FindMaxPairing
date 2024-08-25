module Pairing where

import GraphDataStructures
import PairingUtil


-- Vezme síť (s toky 0), jméno zdrojového vrcholu, jméno koncového vrcholu a vrátí síť s maximálním tokem
fordfalk :: (Eq a, Show a) => Graf a -> a -> a -> Graf a
fordfalk gs zt st = grafy rs
  where
    rs = fordfalk' (rezervy gs) zt st

-- Vezme list vrcholů jedné partity a seznamu vrcholů druhé partity, do kterých vede z prvního vrcholu hrana
parovani :: [(String, [String])] -> [(String, String)]
parovani xs = grafnapary maxtok
  where
    gs = rozsirGraf xs
    maxtok = fordfalk gs z s