derivation => syntax tree -t
syntax tree => derivation -f
nonterminal N => n's children in a syntax tree -f
terminal a => a's parent in a syntax tree -f

data A = Forward Int | Left | Turn D | Seq A A 
data D = North | East | Degree Int
p = Seq (Forward 5) (Turn (Degree 60))

a.string|string|string
b. Int|?Int or RE|Int
c. Int bool|Int bool|Int
d. [bool]|[bool]|Int
e. TE|[a]or TE
f. TE|TE,[bool]|bool