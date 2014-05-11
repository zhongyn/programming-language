---------------------------------------------------
--Exercise 2. Shape Language
---------------------------------------------------

--Syntax of shape
data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show


--Semantic domanin
type BBox = (Int,Int)
type Pixel = (Int,Int)
type Image = [Pixel]


--Define bounding box of a shape
bbox :: Shape -> BBox
bbox s = ((maxx i)-(minx i)+1,(maxy i)-(miny i)+1)
   where i = sem s


--Semantic function
sem :: Shape -> Image 
sem X           = [(1,1)]
sem (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem s2] 
                 where d1 = sem s1
sem (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem s1] 
                 where d2 = sem s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

minx :: [Pixel] -> Int
minx = minimum . map fst

miny :: [Pixel] -> Int
miny = minimum . map snd

--Test
s1 = LR (TD X X) X
p1 = sem s1
bb1 = bbox s1

s2 = TD X s1
p2 = sem s2
bb2 = bbox s2


--Define rect type checker
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (LR s1 s2)| hei s1==hei s2 = Just (wid s1+wid s2, hei s1)
rect (TD s1 s2)| wid s1==wid s2 = Just (wid s1, hei s1+hei s2)
rect _ = Nothing

hei :: Shape -> Int
hei = snd.bbox

wid :: Shape -> Int
wid = fst.bbox

--Test
s3 = LR X X 
s4 = TD s3 s3
s5 = LR s4 s4
rs3 = rect s3
rs4 = rect s4
rs5 = rect s5













































