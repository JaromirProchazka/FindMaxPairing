module Pairing where

import PairingUtil


-- Vezme list vrcholů jedné partity a seznamu vrcholů druhé partity, do kterých vede z prvního vrcholu hrana
parovani :: [(String, [String])] -> [(String, String)]
parovani xs = grafnapary maxtok
  where
    gs = rozsirGraf xs
    maxtok = fordfalk gs z s