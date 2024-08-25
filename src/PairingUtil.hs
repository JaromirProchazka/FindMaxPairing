module PairingUtil where

import GraphDataStructures
import Data.List
-- import Debug.Trace
import Debug.Trace (trace)

-- set, if you want to log debug messages
is_debuging :: Bool
is_debuging = False


trace' :: (Show a) => a -> a
trace' x 
  | (is_debuging) = trace (show x) x
  | otherwise = x

replace :: [b] -> (b -> Bool) -> (b -> b) -> [b]
replace [] _ _ = []
replace (b : bs) predch mapTo
  | (predch b) = (mapTo b) : bs
  | otherwise = b : (replace bs predch mapTo)

rezervy :: Graf a -> Rezervy a
rezervy g =
  map
    ( \(v, es) ->
        (v, (map (\(nextv, t, k) -> (nextv, k, t, 0)) es))
    )
    g

grafy :: Rezervy a -> Graf a
grafy rs =
  map
    ( \(u, es) ->
        (u, (map (\(v, c, uv, vu) -> (v, uv - vu, c)) es))
    )
    rs

resetReached :: Rezervy a -> Rezervy a
resetReached rs = map (\(v, es) -> (v, es)) rs

-- vrátí rezervu hrany
rezerva :: RezervaHrana a -> Int
rezerva (_, c, uv, vu) = c - uv + vu

-- najde následovníky vrcholu v síti, kteří nejsou na nasycené hraně
getNextV :: (Eq a, Show a) => Rezervy a -> a -> Path a -> [RezervaHrana a]
getNextV [] _ _ = []
getNextV ((v, es) : g) st p
  | (v == st) = filtered
  | otherwise = getNextV g st p
  where
    filtered = trace' (filter (\(e, c, uv, vu) -> 0 < rezerva (e, c, uv, vu) && (filter (\(pv, _, _, _) -> pv == e) p) == []) es)

-- bfsPath :: Graf -> Koncový vrchol -> Queue dvojic vrchol a cesta do něj obsahující startovací vrchol -> nenasycená cesta
bfsPath :: (Eq a, Show a) => Rezervy a -> a -> a -> (Int, Path a)
bfsPath r st e = bfsBody r e (enqueue (st, [], maxBound :: Int) emptyQueue)

-- hledá nenasycenou cestu mezi vrcholy
-- Impl: dokud není Q prázdná
-- deque
-- případně přidej do meziv.
-- enque všechny (následníky, dosavadní cesta)
bfsBody :: (Eq a, Show a) => Rezervy a -> a -> SQueue (a, Path a, Int) -> (Int, Path a)
bfsBody g e q
  | (isEmpty q) = (0, [])
  | (v == e) = (i, reverse p) -- případně přidej do meziv.
  | otherwise = bfsBody g e newQ -- jeden z výsledků nenalezen
  where
    ((v, p, i), dq) = dequeue q
    -- následníci
    -- next_es :: [RezervaHrana a]
    next_es = getNextV g v p
    -- vytvoří seznam dvojic (další vrchol, nynější cesta s tímto vrcholem)
    -- nextsQs :: [(a, Path a, Int)]
    nextsQs = map (\(vv, c, uv, vu) -> (vv, ((vv, c, uv, vu) : p), min (rezerva (vv, c, uv, vu)) i)) next_es
    -- výsledná que jednoho kroku bfs
    newQ = enqueueLs nextsQs dq -- enque všechny (následníky, dosavadní cesta)

-- Jeden krok FF, dostane jednu nenasycenou cestu a nasytí ji
-- ffKrok :: Graf rezerv -> zdroj -> cesta a její min rezerva -> Nový Graf Rezerv
ffKrok :: (Eq a, Show a) => Rezervy a -> a -> (Int, Path a) -> Rezervy a
ffKrok rs _ (_, []) = rs
ffKrok rs zt (epsylon, ((v, c, uv, vu) : p)) = ffKrok new_rs v (epsylon, p)
  where
    delta = min vu epsylon
    new_rs = updateTok rs zt (v, c, uv, vu) epsylon delta

-- Vezme hranu toku na nenasycené cestě a nasytí
-- updateTok ((s,es,b):rs) u (v, c, uv, vu) epsylon delta
updateTok :: (Eq a, Show a) => Rezervy a -> a -> RezervaHrana a -> Int -> Int -> Rezervy a
updateTok [] _ _ _ _ = []
updateTok ((st, es) : rs) u (v, c, uv, vu) epsylon delta
  | (st == u) = (st, new_es) : rs
  | otherwise = (st, es) : updateTok rs u (v, c, uv, vu) epsylon delta
  where
    new_es =
      replace
        es
        (\(v1, _, _, _) -> v1 == v)
        (\(v1, c1, uv1, vu1) -> (v1, c1, uv1 + epsylon - delta, vu1 - delta))

fordfalk' :: (Eq a, Show a) => Rezervy a -> a -> a -> Rezervy a
fordfalk' rs zt st
  | (fst nenasycenaCesta == 0) = rs
  | otherwise = fordfalk' new_rs zt st
  where
    nenasycenaCesta = bfsBody rs st (enqueue (zt, [], maxBound :: Int) emptyQueue)
    new_rs = ffKrok rs zt (trace' nenasycenaCesta)

-- label pro zdrojový vrchol
z :: String
z = "$z~"

-- label pro stokový vrchol
s :: String
s = "$s~"

combineAndRemoveDuplicates :: (Eq a) => [[a]] -> [a]
combineAndRemoveDuplicates = nub . concat

-- Vezme seznam vrcholů první partity, a následujících vrcholů druhé partity a přidá zdroj a stok
rozsirGraf :: [(String, [String])] -> Graf String
rozsirGraf gs = (rozsirGraf' gs (z, []) s) ++ (map (\v -> (v, [(s, 0, 1)])) vs_s)
  where
    vs_s = combineAndRemoveDuplicates (map (\(_, es) -> es) gs)

-- rozsirGraf' gs u (v, t, c)
rozsirGraf' :: (Eq a, Show a) => [(a, [a])] -> (a, [a]) -> a -> Graf a
rozsirGraf' [] (zt, zs) st = [(zt, (map (\v -> (v, 0, 1)) zs)), (st, [])]
rozsirGraf' ((u, vs) : gs) (zt, zs) st = (u, (map (\v -> (v, 0, 1)) vs)) : rozsirGraf' gs (zt, u : zs) st

-- převede max toky bipartitního grafu na array párů vrcholů představující párování
grafnapary :: Graf String -> [(String, String)]
grafnapary [] = []
grafnapary ((u, evs) : gs)
  | (u == z || u == s || vs == []) = grafnapary gs
  | otherwise = (u, head vs) : grafnapary gs
  where
    es = filter (\(v, t, _) -> t == 1 && v /= z && v /= s) evs
    vs = map (\(v, _, _) -> v) es