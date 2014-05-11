Note: This file can be directly loaded into ghci.


EXERCISE 1 [10+10+20 = 40 Points]

(a) [10 Points]

We need three data types to represent the nonterminals cmd, mode, and
pos. Since the nonterminals pars and vals define lists of names and
nums, respectively, we can use Haskell lists to represent those.

> data Cmd = Pen Mode
>          | MoveTo Pos Pos
>          | Def  String [String] Cmd
>          | Call String [Int] 
>          | Seq Cmd Cmd
> 
> data Mode = Up | Down
> 
> data Pos = I Int | Var String

One can also represent the lists using additional data types for the
nonterminals pars and vals. 

data Pars = Par String Pars | NoPars

data Vals = Val Int Vals | NoVals

In that case the definition of Cmd has to be changed as follows.

data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def  String Pars Cmd
         | Call String Vals 
         | Seq Cmd Cmd

However, the first definition is definitely better for two reasons:
First, it saves the definition of additional data types, and second,
it allows the reuse of list operations when manipulating parameter and
value lists in abstract syntax trees.

To make the following examples more readable when used in ghci, here is 
a simple Mini Logo pretty printer. The pretty printer adds curly brackets
around bodies of macros.

> ppCmd :: String -> Cmd -> String
> ppCmd s (Pen Up)     = s++"pen up"
> ppCmd s (Pen Down)   = s++"pen down"
> ppCmd s (MoveTo x y) = s++"moveto ("++show x++","++show y++")"
> ppCmd s (Def n xs c) = "def "++n++"("++ppPars xs++") {\n"++
>                        ppCmd "  " c++"\n}"
> ppCmd s (Call n xs)  = "call "++n++"("++ppArgs xs++")"
> ppCmd s (Seq c c')   = ppCmd s c++";\n"++
>                        ppCmd s c'
> 
> printList :: (a -> String) -> String -> [a] -> String
> printList f s [x]    = f x
> printList f s (x:xs) = f x++s++printList f s xs
>
> ppPars :: [String] -> String
> ppPars xs = printList id "," xs
> 
> ppArgs :: [Int] -> String
> ppArgs xs = printList show "," xs
> 
> instance Show Cmd where
>   show = ppCmd ""
> 
> instance Show Pos where
>   show (I i)   = show i
>   show (Var s) = s


(b) [10 Points]

The macro definition in concrete Mini Logo syntax looks as follows.

  def vector (x1,y1,x2,y2) pen up; moveto (x1,y1); pen down; moveto (x2,y2)

The abstract syntax is represented as a value of the data type Cmd.

> vector :: Cmd
> vector = Def "vector" ["x1","y1","x2","y2"] 
>          (Seq (Pen Up)
>               (Seq (MoveTo (Var "x1") (Var "y1"))
>                    (Seq (Pen Down)
>                         (MoveTo (Var "x2") (Var "y2")) )))

A representation that uses the explicit data type representation for
the nonterminals pars and vals looks as follows.

vector :: Cmd
vector = Def "vector" 
         (Par "x1" (Par "y1" (Par "x2" (Par "y2" NoPar))))
         (Seq (Pen Up)
              (Seq (MoveTo (Var "x1") (Var "y1"))
                   (Seq (Pen Down)
                        (MoveTo (Var "x2") (Var "y2")) )))

Writing nested applications of Seq (and Par, etc.) can get quite
tedious. Here are two alternatives that show how to simplify writing
syntax trees.

First, any binary function or constructor can be written using infix
notation by enclosing it in backquotes. E.g.  instead of div 7 3 we
can write 7 `div` 3. Since Seq is a binary constructor, we can use
this notation to write a sequence of Cmds in a way that is closer to
the concrete syntax.

> vector1 :: Cmd
> vector1 = Def "vector" ["x1","y1","x2","y2"] 
>           (Pen Up                       `Seq`
>            MoveTo (Var "x1") (Var "y1") `Seq`
>            Pen Down                     `Seq`
>            MoveTo (Var "x2") (Var "y2"))

A representation that uses the explicit data type representation for
the nonterminals pars and vals looks as follows.

vector1 :: Cmd
vector1 = Def "vector" ("x1" `Par` "y1" `Par` "x2" `Par` "y2" `Par` NoPar) 
          (Pen Up                       `Seq`
           MoveTo (Var "x1") (Var "y1") `Seq`
           Pen Down                     `Seq`
           MoveTo (Var "x2") (Var "y2"))

Another alternative is to put all commands into a Haskell list and
use a function to convert this list into a tree built from Seq
constructors.

> vector2 :: Cmd
> vector2 = Def "vector" ["x1","y1","x2","y2"] 
>           (foldr1 Seq [Pen Up,  MoveTo (Var "x1") (Var "y1"),
>                        Pen Down,MoveTo (Var "x2") (Var "y2")])

The Haskell function foldr1 takes a binary operation (the constructor
Seq in this case) and a list and combines all the elements of the list
with this operation.



(c) [20 Points]

Here is a solution that creates the command sequences for the topmost
step and connects it (using Seq) to the command sequence for the
remaining stair. We use the representation shown in vector2 in part
(b).

> steps :: Int -> Cmd
> steps 1 = foldr1 Seq [Pen Up,  MoveTo (I 0) (I 0),
>                       Pen Down,MoveTo (I 0) (I 1),
>                                MoveTo (I 1) (I 1)]
> steps n = Seq (steps (n-1))
>               (foldr1 Seq [MoveTo (I (n-1)) (I n),
>                            MoveTo (I n)     (I n)])



EXERCISE 2  [10+5+20 = 35 Points]

(a) [10 Points]

> data Circuit = Circ Gates Links 
> 
> data Gates   = Gate Int GateFn Gates | EmptyGate
> 
> data GateFn  = And | Or | XOr | Not
> 
> data Links   = Link Int Int Int Int Links | EmptyLinks

The above solution uses the direct encoding of grammar productions by data
types. We can simplify the definition by observing that both constructors
EmptyGate and EmptyLinks are only used to represent lists of gates and links.
Therefore an alternative definition could be obtained by defining data
types representing individual gates and links and using type definitions
to represent the list structures.

data Gate1 = Gate Int GateFn 

data Link  = Link Int Int Int Int

type Gates = [Gate1]

type Links = [Link]



(b) [5 Points]

> halfAdder = Circ gates links
>             where  gates = Gate 1 XOr (Gate 2 And EmptyGate)
>                    links = Link 1 1 2 1 (Link 1 2 2 2 EmptyLinks)

Using the alternative representation for lists shown in (a) we would
define halfAdder as follows.

halfAdder = Circ [Gate 1 XOr,Gate 2 And] [Link 1 1 2 1, Link 1 2 2 2]



(c) [20 Points]

We define a pretty printing function for each nonterminal. ppGt and
ppLink are defined as auxiliary functions. (Note: This pretty printer
does not print a final semicolon, which the grammar in the homework
produces. But this is a mistake in the original grammar.)

> ppCircuit :: Circuit -> String
> ppCircuit (Circ gs ls) = ppGates gs++";\n"++ppLinks ls
> 
> ppGt :: Int -> GateFn -> String
> ppGt i g = show i++":"++ppGateFn g
> 
> ppGates :: Gates -> String
> ppGates EmptyGate            = ""
> ppGates (Gate i g EmptyGate) = ppGt i g
> ppGates (Gate i g gs)        = ppGt i g++";\n"++ppGates gs
> 
> ppLink :: Int -> Int -> Int -> Int -> String
> ppLink fg fp tg tp = "from "++show fg++"."++show fp++" to "++
>                        show tg++"."++show tp
> 
> ppLinks :: Links -> String
> ppLinks EmptyLinks = ""
> ppLinks (Link s1 s2 t1 t2 EmptyLinks) = ppLink s1 s2 t1 t2
> ppLinks (Link s1 s2 t1 t2 ls) = ppLink s1 s2 t1 t2++";\n"++ppLinks ls
> 
> ppGateFn :: GateFn -> String
> ppGateFn And = "and"
> ppGateFn Or  = "or "
> ppGateFn XOr = "xor"
> ppGateFn Not = "not"

To install the pretty printer so that values of those data types will
be automatically printed using them, we can define the data types as
instances of the Show class as follows.

> instance Show Circuit where
>   show = ppCircuit 
> 
> instance Show Gates where
>   show = ppGates
> 
> instance Show Links where
>   show = ppLinks
> 
> instance Show GateFn where
>   show = ppGateFn



EXERCISE 3  [5+6+14 = 25 Points]

Here are the data type definitions to allow this file to be loaded into
Haskell.

> data Expr = N Int
>           | Plus  Expr Expr
>           | Times Expr Expr
>           | Neg Expr
>             deriving Show
> 
> data Op = Add | Multiply | Negate deriving Show
> data Exp = Num Int
>          | Apply Op [Exp]
>            deriving Show



(a) [5 Points]

> ex =  Apply Multiply [Apply Negate [Apply Add [Num 3, Num 4]],
>                       Num 7]



(b) [6 Points]

Advantage: A potential advantage of the new syntax is that it is a bit easier
to extend by new operations: Just add a constructor to Op, whereas adding a
constructor to Expr requires also to be explicit about the exact number of
arguments. In particular, for overloaded operations, the new representation
is simpler since in the old representation multiple constructors would be
needed.

Disadvantage: The new syntax does not enforce the correct number of arguments
for operations, for example, it doesn't restrict Negate to a single operand
or Plus to two operands. For example, what is the meaning of (Apply Negate
[1,2]) or Apply Plus []? So the additional work required for the first
representation (the disadvantage) pays off in the form of better consistency
checking.



(c) [14 Points]

> translate :: Expr -> Exp
> translate (N i)        = Num i
> translate (Plus e e')  = Apply Add (map translate [e,e'])
> translate (Times e e') = Apply Multiply (map translate [e,e'])
> translate (Neg e)      = Apply Negate [translate e]

