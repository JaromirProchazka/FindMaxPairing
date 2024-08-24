module PairingUtil where

import Data.List
-- import Debug.Trace
import Data.String (IsString)
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
replace (b : bs) pred mapTo
  | (pred b) = (mapTo b) : bs
  | otherwise = b : (replace bs pred mapTo)

-- Vrchol -> [(Následníci, Toky, Kapacity)]
type GrafHrana a = (a, Int, Int)

type Graf a = [(a, [GrafHrana a])]

-- do vrcholu, kapacita hrany, rezerva po hraně, rezerva proti hraně
type RezervaHrana a = (a, Int, Int, Int)

-- Rezervy :: [vrchol, [hrany], reached]
type Rezervy a = [(a, [RezervaHrana a])]

type Path a = [RezervaHrana a]

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
rezerva (v, c, uv, vu) = c - uv + vu

class Queue q where
  emptyQueue :: q a
  isEmpty :: q a -> Bool
  enqueue :: a -> q a -> q a
  dequeue :: q a -> (a, q a)
  enqueueLs :: [a] -> q a -> q a

data SQueue a = SQueue [a] [a]

instance (Eq a) => Eq (SQueue a) where
  SQueue f b == SQueue otherF otherB = f ++ (reverse b) == otherF ++ (reverse otherB)

-- Implementing the Queue instance
instance Queue SQueue where
  emptyQueue = SQueue [] []
  isEmpty (SQueue [] []) = True
  isEmpty (SQueue f b) = False
  enqueue x (SQueue f b) = SQueue f (x : b)
  enqueueLs [] qs = qs
  enqueueLs (x : xs) qs = enqueueLs xs (enqueue x qs)
  dequeue (SQueue [] []) = error "Can't dequeue an empty Queue!"
  dequeue (SQueue (f : fs) b) = (f, SQueue fs b)
  dequeue (SQueue f b) = ((head newF), (SQueue (tail newF) []))
    where
      newF = f ++ (reverse b)

instance (Show a) => Show (SQueue a) where
  show (SQueue f b) = "q" ++ show (f ++ (reverse b)) ++ "\n"

-- qs = enqueue 7 (enqueue 5 (enqueue 3 emptyQueue)) :: SQueue Int
-- qs2 = dequeue qs
-- deqed = (enqueue 6969 (snd (dequeue (queue_of_nums 3 1000 :: (SQueue Int)))))

-- f)
instance Functor SQueue where
  fmap func (SQueue f b) = SQueue (map func f) (map func b)

queue_of_nums :: (Queue q) => Int -> Int -> q Int
queue_of_nums x y = queue_of_nums' emptyQueue x y

queue_of_nums' :: (Queue q) => q Int -> Int -> Int -> q Int
queue_of_nums' que x y
  | (x > y) = que
  | otherwise = queue_of_nums' (enqueue x que) (x + 1) y

-- najde následovníky vrcholu v síti, kteří nejsou na nasycené hraně
getNextV :: (Eq a, Show a) => Rezervy a -> a -> Path a -> [RezervaHrana a]
getNextV [] _ _ = []
getNextV ((v, es) : g) s p
  | (v == s) = filtered
  | otherwise = getNextV g s p
  where
    filtered = trace' (filter (\(e, c, uv, vu) -> 0 < rezerva (e, c, uv, vu) && (filter (\(pv, _, _, _) -> pv == e) p) == []) es)

-- bfsPath :: Graf -> Koncový vrchol -> Queue dvojic vrchol a cesta do něj obsahující startovací vrchol -> nenasycená cesta
bfsPath :: (Eq a, Show a) => Rezervy a -> a -> a -> (Int, Path a)
bfsPath r s e = bfsBody r e (enqueue (s, [], maxBound :: Int) emptyQueue)

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
    nextsQs = map (\(v, c, uv, vu) -> (v, ((v, c, uv, vu) : p), min (rezerva (v, c, uv, vu)) i)) next_es
    -- výsledná que jednoho kroku bfs
    newQ = enqueueLs nextsQs dq -- enque všechny (následníky, dosavadní cesta)

-- Jeden krok FF, dostane jednu nenasycenou cestu a nasytí ji
-- ffKrok :: Graf rezerv -> zdroj -> cesta a její min rezerva -> Nový Graf Rezerv
ffKrok :: (Eq a, Show a) => Rezervy a -> a -> (Int, Path a) -> Rezervy a
ffKrok rs _ (_, []) = rs
ffKrok rs z (epsylon, ((v, c, uv, vu) : p)) = ffKrok new_rs v (epsylon, p)
  where
    delta = min vu epsylon
    new_rs = updateTok rs z (v, c, uv, vu) epsylon delta

-- Vezme hranu toku na nenasycené cestě a nasytí
-- updateTok ((s,es,b):rs) u (v, c, uv, vu) epsylon delta
updateTok :: (Eq a, Show a) => Rezervy a -> a -> RezervaHrana a -> Int -> Int -> Rezervy a
updateTok [] _ _ _ _ = []
updateTok ((s, es) : rs) u (v, c, uv, vu) epsylon delta
  | (s == u) = (s, new_es) : rs
  | otherwise = (s, es) : updateTok rs u (v, c, uv, vu) epsylon delta
  where
    new_es =
      replace
        es
        (\(v1, c1, uv1, vu1) -> v1 == v)
        (\(v1, c1, uv1, vu1) -> (v1, c1, uv1 + epsylon - delta, vu1 - delta))

fordfalk' :: (Eq a, Show a) => Rezervy a -> a -> a -> Rezervy a
fordfalk' rs z s
  | (fst nenasycenaCesta == 0) = rs
  | otherwise = fordfalk' new_rs z s
  where
    nenasycenaCesta = bfsBody rs s (enqueue (z, [], maxBound :: Int) emptyQueue)
    new_rs = ffKrok rs z (trace' nenasycenaCesta)

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
    vs_s = combineAndRemoveDuplicates (map (\(v, es) -> es) gs)

-- rozsirGraf' gs u (v, t, c)
rozsirGraf' :: (Eq a, Show a) => [(a, [a])] -> (a, [a]) -> a -> Graf a
rozsirGraf' [] (z, zs) s = [(z, (map (\v -> (v, 0, 1)) zs)), (s, [])]
rozsirGraf' ((u, vs) : gs) (z, zs) s = (u, (map (\v -> (v, 0, 1)) vs)) : rozsirGraf' gs (z, u : zs) s

-- převede max toky bipartitního grafu na array párů vrcholů představující párování
grafnapary :: Graf String -> [(String, String)]
grafnapary [] = []
grafnapary ((u, evs) : gs)
  | (u == z || u == s || vs == []) = grafnapary gs
  | otherwise = (u, head vs) : grafnapary gs
  where
    es = filter (\(v, t, _) -> t == 1 && v /= z && v /= s) evs
    vs = map (\(v, t, _) -> v) es

-- Vezme síť (s toky 0), jméno zdrojového vrcholu, jméno koncového vrcholu a vrátí síť s maximálním tokem
fordfalk :: (Eq a, Show a) => Graf a -> a -> a -> Graf a
fordfalk gs z s = grafy rs
  where
    rs = fordfalk' (rezervy gs) z s