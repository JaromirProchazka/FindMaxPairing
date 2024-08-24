module GraphDataStructures where

import Data.List
-- import Debug.Trace
import Debug.Trace (trace)


-- Vrchol -> [(Následníci, Toky, Kapacity)]
type GrafHrana a = (a, Int, Int)

type Graf a = [(a, [GrafHrana a])]

-- do vrcholu, kapacita hrany, rezerva po hraně, rezerva proti hraně
type RezervaHrana a = (a, Int, Int, Int)

-- Rezervy :: [vrchol, [hrany], reached]
type Rezervy a = [(a, [RezervaHrana a])]

type Path a = [RezervaHrana a]

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
  isEmpty (SQueue _ _) = False
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