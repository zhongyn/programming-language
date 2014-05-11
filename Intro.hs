
plus :: Int -> Int -> Int
plus c x = x+c

minus :: Int -> Int -> Int
minus x y = x-y

flp :: (a -> b -> c) -> b -> a -> c
--flp f = \y x->f x y
flp f y x = f x y

data Geo = Rect Int Int
         | Circ Int
         | Sqr Int
         --deriving (Show,Eq)

instance Eq Geo where
  Rect x y == Rect a b = x==a && y==b
  Sqr x    == g        = Rect x x == g
--Sqr x    == Rect a b = x==a && x==b

r = Rect 3 4
s = Rect 4 5

ones = 1:ones

nats = 1:map (+1) nats

{-
map f (x:xs) = f x:map f xs

map f (map g xs) = map (f . g) xs

  nats 
= 1:map succ nats
= 1:map succ (1:map succ nats)
= 1:succ 1:map succ (map succ nats)
= 1:2:map succ (map succ nats)
= 1:2:map (succ . succ) nats
= 1:2:map (+2) nats
= 1:2:map (+2) (1:2:map (+2) nats)
= 1:2:3:4:map (+2) (map (+2) nats)
= 1:2:3:4:map ((+2) .(+2)) nats
= 1:2:3:4:map (+4) nats
-}

evens = 2:map (+2) evens
evens' = map (*2) nats
evens'' = filter even nats

{-
1  1  2  3  5  8 13 21 ...  fibs
1  2  3  5  8 13 21 ...     tail fibs
----------------------------------------------
2  3  5  8 13 21 34 ...
-}

fibs = 1:1:zipWith (+) fibs (tail fibs)
 
data Grade = A | B | C | D | F
			deriving (Eq, Show, Ord, Enum)

pass :: Grade -> Bool
--pass A = True
--pass B = True
--pass C = True
--pass _ = False
pass g = g < D

passingGrades :: [Grade]
passingGrades = filter pass [A .. F]

-- type Age = Int
data Age = Year Int
			deriving (Eq,Show,Ord)

birthday :: Age -> Age
birthday (Year y) = Year (y+1)

drinkingAge :: Age
drinkingAge = Year 21

alcoholOK :: Age -> Bool
alcoholOK a = a >= drinkingAge

data Tree = Node Int Tree Tree
          | Leaf 
           deriving (Show, Eq) 

t = Node 3 (Node 1 Leaf Leaf) 
           (Node 5 Leaf Leaf)

t' = Node 7 t t

find :: Int -> Tree -> Bool
find _ Leaf = False
find i (Node j l r) | i==j      = True
                    | otherwise = find i l || find i r  -- plain tree
--                    | i<j       = find i l  -- binary search tree
--                    | otherwise = find i r

{-
if e then True else False  ==  e

if True then True else False -> True
if False then True else False -> False
-}




