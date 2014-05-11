----------------------------------
--CS381 HW2
----------------------------------
--Yaonan Zhong
--Xiangyu Wang
--Xiaowei Zhang
-----------------------------------
--April 29, 2014
-----------------------------------

----------------------------------------------------
--Exercise 1. A Stack Language
----------------------------------------------------
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving (Show, Eq)

type Stack = [Int]
type D = Stack -> Stack

--Define semCmd :: Cmd -> Stack -> Stack
semCmd :: Cmd -> D
semCmd (LD i) xs = [i]++xs
semCmd ADD xs = errTest ADD xs
semCmd MULT xs = errTest MULT xs
semCmd DUP xs = errTest' DUP xs

--Check the length of stack for ADD and MULT
errTest :: Cmd -> D
errTest f xs
      | length xs>=2 = if f==ADD then [a+b]++c else [a*b]++c
      | otherwise = error "less than two elements"
      where (a:b:c) = xs 

--Check the length of stack for DUP
errTest' :: Cmd -> D
errTest' f xs
       | length xs>=1 = [a]++xs
       | otherwise = error "less than one element"
       where a:_ = xs

--Call a list of commands
sem :: Prog -> D
sem [] c = c
sem (o:os) c = sem os (semCmd o c) 

-----------------------------
--Test
-----------------------------
op = [LD 3,DUP,ADD,DUP,MULT]
opp = [LD 3,ADD]
hi = sem op [1,2,3,4,5]

