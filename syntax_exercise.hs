sentence := noun verb noun
			| sentence and sentence

binary := 0 binary | 1 binary
binary := nothing

bool = not bool
bool = T
bool = F

data bin = digit | digit bin
data digit = 0|1

data bin = R3 digit | R4 digit bin
data digit = R1 0 | R2 1

101 = R4 (R2 1) (R4 (R1 0) (R3 (R2 1)))

data con = 0|1
data reg = A|B|C
data op = (MOV con|reg TO reg)
		  | (INC reg BY con|reg)


data Cond = T| Not Cond
data Stmt = While Cond Stmt
			| Noop

ppCond :: Cond -> String
ppCond T = "T"
ppCond Not b = "not("++ppCond b++")"

ppStmt :: Stmt -> String
ppStmt Noop = "noop"
ppStmt While a b = "while "++ppCond a++" {"++ppStmt b++"}"