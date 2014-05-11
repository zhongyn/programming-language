----------------------------------
--CS381 HW2
----------------------------------
--Yaonan Zhong
--Xiangyu Wang
--Xiaowei Zhang
-----------------------------------
--April 29, 2014
-----------------------------------

----------------------------------
--Exercise 3. Mini Logo
----------------------------------
--import SVG (ppLines)

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd
         deriving Show

data Mode = Up|Down deriving Show

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS (Pen m) (a,b,c) = ((m,b,c),[(b,c,b,c)])
semS (MoveTo i j) (a,b,c) = ((a,i,j),[(b,c,i,j)])
semS (Seq s k) st = (fst kl, snd sl++snd kl)
                  where sl = semS s st   
                        kl = semS k (fst sl)


sem' :: Cmd -> Lines
sem' c = snd (semS c (Up,0,0))

-------------------------------------
--Test
-------------------------------------
l = semS (Seq (Seq (Pen Down) (MoveTo 1 1)) (MoveTo 2 2)) (Up,0,0)
prog = Seq(Seq (Seq (Pen Down) (MoveTo 1 1)) (MoveTo 2 2))(MoveTo 2 5)
--pp = ppLines (sem' prog)
