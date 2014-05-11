---------------------------
--CS381 HW3
---------------------------
--Yaonan Zhong
--Xiaowei Zhang
--Xiangyu Wang
---------------------------
--May 6, 2014
---------------------------

-------------------------------------------------------------------------
--Exercise 1. A rank-basd type systems for the stack language
-------------------------------------------------------------------------

type Prog = [Cmd]
data Cmd = LD Int
		 | ADD
		 | MULT
		 | DUP
		 | INC
		 | SWAP
		 | POP Int
		 deriving Show

type Rank = Int
type CmdRank = (Int,Int)


--Define rankC :: Cmd -> CmdRank
rankC :: Cmd -> CmdRank
rankC (LD i) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP i) = (i,0)


--Define rankP :: Prog -> Maybe Rank
rankP :: Prog -> Maybe Rank
rankP = foldl (\acc x -> if (comp acc (a x)) then (Just ((getR acc)-(a x)+(b x))) else Nothing) (Just 0)
   where (a,b) = (fst.rankC,snd.rankC)
         getR (Just r) = r
         comp mr r = case (mr,r) of
                     ((Just i),j) -> i>=j
                     _            -> False


--Define semCmd :: Cmd -> Maybe Stack -> Maybe Stack 
type Stack = [Int]
semCmd :: Cmd -> Maybe Stack -> Maybe Stack
semCmd (LD i) (Just s) = Just (i:s)
semCmd DUP (Just vs@(v:_)) = Just (v:vs)
semCmd ADD (Just (v1:v2:vs)) = Just (v1+v2:vs)
semCmd MULT (Just (v1:v2:vs)) = Just (v1*v2:vs)
semCmd INC (Just (v:vs)) = Just (v+1:vs)
semCmd SWAP (Just (v1:v2:vs)) = Just (v2:v1:vs) 
semCmd (POP i) (Just s) = Just (drop i s)
semCmd _ _ = Nothing


--Define sem :: Prog -> Maybe Stack -> Maybe Stack
sem :: Prog -> Maybe Stack -> Maybe Stack
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)


--Define semStatTC :: Prog -> Maybe Stack -> Maybe Stack
typeSafe :: Prog -> Bool
typeSafe c = rankP c /= Nothing

semStatTC :: Prog -> Maybe Stack -> Maybe Stack
semStatTC c s| typeSafe c = sem c s
			 | otherwise = Nothing


--Function sem can be simplified to the type: Prog -> Stack,
--since we already have the static type checker "rankP". The
--dynamic type checking in sem is no more necessary.


--Test
cs = [LD 2,DUP,INC,ADD,LD 3,SWAP,POP 1]
err = [LD 1,ADD]
re = rankP cs
te = rankP err


















