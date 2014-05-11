----------------------------------------------------------------
--Exercise 3. Parametric Polymorphism
----------------------------------------------------------------
(a) Consider the functions f and g , which are given by the following two function definitions.
	f x y = if null x then [y] else x
	g x y = if not (null x) then [] else [y]

(1) What are the types of f and g ?

	f :: [a] -> a -> [a] 
	g :: [a] -> b -> [b]


(2) Explain why the functions have these types.

	"Null x" decides the type of x is List.

	Under static typing, the return type of function f should be precisely one type, so [y]
	and x has the same type. Since x is a list, y should have the same type as the element 
	in x.

	In function g, the return value is [] or [y]. And the return type is decided by [y], since 
	an empty list can have an element of any type. The only constraint is the type of [y].

(3) Which type is more general?

	g :: [a] -> b -> [b] is more general, since x and y can have different types.

(4) Why do f and g have different types?

	The input and output of g can have different types with f.

------------------------------

(b) Find a (simple) definition for a function h that has the following type.
	h :: [b] -> [(a, b)] -> [b]
	Note that the goal of this part of the exercise is not to find a particularly useful function, but a
	function definition that has the given type signature. More precisely, you should give a definition
	for h without a type annotation, for which GHCi will then infer the shown type. A perfectly
	valid way to approach this exercise is to experiment with function definitions, without giving type
	declarations, and have GHCi infer the type using the :t interpreter command.

	h (b1:bs) ((a,b2):ab) = if Ture then [b1] else [b2]

------------------------------------

(c) Find a (simple) definition for a function k that has the following type.
	k :: (a -> b) -> ((a -> b) -> a) -> b
	All remarks from part (b) apply here as well.

	k f g = f (g f)  

------------------------------------
(d) Can you define a function of type a -> b ?
	If yes, explain your definition. If not, explain why it is so difficult.

	Not, since we can not infer the return type from the input type.


