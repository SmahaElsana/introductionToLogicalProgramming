/**** I, Smaha Elsana (204841506) assert that the work I submitted is entirely my own.
I have not received any part from any other student in the class (or other source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. *****/

% ################################################### BELOW RULES WERE GIVEN IN CLASS ##################################################3

append([_|Xs], Ys, [_|Zs]) :-
	append(Xs, Ys, Zs).
	append([], Xs, Xs).

select([K|Ks], Ns):-
	append(_, [K|Rest], Ns),
    	select(Ks, Rest).
	select([],_).

nat(0).
nat(s(X)) :- 
	nat(X).

add(0, X, X) :- 
	nat(X).
add(s(X), Y, s(Z)) :-
	add(X,Y,Z).

leq(0,_).
leq(s(X), s(Y)) :-
	leq(X,Y).

lt(X,Y) :- 
	add(s(_), X, Y).

times(0,X,0) :- 
	nat(X).
	
times(s(X),Y,Z) :-
	times(X,Y,W),
	add(W,Y,Z).

list([]).
list([_|Xs]) :- list(Xs).

member(X, [X|_]).
member(X, [_|Xs]) :- 
	member(X,Xs).

binaryList([]).
binaryList([X|Xs]) :-
	binaryList(Xs),
	member(X, [0,1]).

bubbleSort(Xs, Ys) :-
	append(A, [X,Y|B], Xs),
	lt(Y,X),
	append(A, [Y,X|B] , Zs),
	bubbleSort(Zs, Ys).

bubbleSort(Xs, Xs) :- 
	sorted(Xs).

sorted([]).
sorted([_]).
sorted([A,B|Xs]):- 
	leq(A,B),
	sorted([B|Xs]).
% ############################################## ALL THE ABOVE RULES WERE GIVEN IN CLASS #########################################################

%###Task 1: Unary Square Root #####

unary_sqrt(0,0).    
unary_sqrt(s(0),s(0)). 
unary_sqrt(N,K):- 
	lt(K,N),
	times(K,K,R1),
	leq(R1,N),
	times(s(K),s(K),R2),
	leq(s(N),R2).   



%###Task 2: Unary Divisor #####

unary_divisor(0,K):-
	nat(K).
	
unary_divisor(N,K):- 
	leq(U,N),
	leq(K,N),
	times(K,U,N).



%###Task 3: Binary Addition ####
%https://okmij.org/ftp/Prolog/Arithm/arithm.pdf

binary_plus_carry1([],[],[1]).
binary_plus_carry1([],[Y|Ys],Zs):- binary_plus([1],[Y|Ys],Zs).
binary_plus_carry1([X|Xs],[],Zs):- binary_plus([X|Xs],[1],Zs).
binary_plus_carry1([X|Xs],[Y|Ys],[Z|Zs]):- binary_plus_carry1_aux(X,Y,Z,Xs,Ys,Zs).

binary_plus_carry1_aux(0,0,1,Xs,Ys,Zs):- binary_plus(Xs,Ys,Zs).
binary_plus_carry1_aux(0,1,0,Xs,Ys,Zs):- binary_plus_carry1(Xs,Ys,Zs).
binary_plus_carry1_aux(1,0,0,Xs,Ys,Zs):- binary_plus_carry1(Xs,Ys,Zs).
binary_plus_carry1_aux(1,1,1,Xs,Ys,Zs):- binary_plus_carry1(Xs,Ys,Zs).

%binary_plus means carry is zero
binary_plus([],[],[]).
binary_plus([],[Y|Ys],[Y|Ys]).
binary_plus([X|Xs],[],[X|Xs]).
binary_plus([X|Xs],[Y|Ys],[Z|Zs]):-binary_plus_aux(X,Y,Z,Xs,Ys,Zs).

%carry is zero
binary_plus_aux(0,0,0,Xs,Ys,Zs):- binary_plus(Xs,Ys,Zs).
binary_plus_aux(0,1,1,Xs,Ys,Zs):- binary_plus(Xs,Ys,Zs).
binary_plus_aux(1,0,1,Xs,Ys,Zs):- binary_plus(Xs,Ys,Zs).
binary_plus_aux(1,1,0,Xs,Ys,Zs):- binary_plus_carry1(Xs,Ys,Zs).




%###Task 4: Binary multiplication #### 
%https://okmij.org/ftp/Prolog/Arithm/arithm.pdf
binary_times([1],[Y|Ys],[Y|Ys]).
binary_times([0],_,[0]).
binary_times(_,[0],[0]).
binary_times([X|Xs],[1],[X|Xs]).

binary_times([0|Xs],[Y|Ys],Zs):- 
				binary_plus([Y|Ys],[Y|Ys],Res), /*same as *2 OR shifting to right*/
    				binary_times(Xs,Res,Zs).

binary_times([1|Xs],[Y|Ys],Zs):- 
				binary_plus([Y|Ys],[Y|Ys],Res1), /*same as *2 OR shifting to right*/
    				binary_plus(Res1,[Y|Ys],Res2),
    				binary_times(Xs,Res2,Zs).



%###Task 5: Primality Testing ####

prime_aux(N,D):- 
	D * D > N,
	D<N,N mod 2 =\= 0.
	
prime_aux(N,D):- 
	D * D < N ,
	N mod 2 =\= 0, /* N is not an even number*/
	N mod D =\= 0, 
	NewD is D + 2, /*increment by 2 to skip even numbers */
	prime_aux(N,NewD).

is_prime(2).
is_prime(3).
is_prime(N):-prime_aux(N,3).




%##Task 6: Test Right Truncatable Primes##

right_prime(2).
right_prime(3).
right_prime(5).
right_prime(7).
right_prime(N):- 
	is_prime(N),  
	Q is div(N, 10),
	right_prime(Q).



%##Task 7: Generate Right Truncatable Primes##

gen(T):- 
	between(2,73939133,X),
    	right_prime(X),
    	T is X.
		
    
    
%###Task 8: Generate Binary Tree from Traversals####

binary_tree(nil).
binary_tree(tree(L,_,R)):-binary_tree(L),binary_tree(R).

% inorder traversal rule is given in class
inorder(nil,[]).
inorder(tree(L,X,R),Xs):- append(Xs1, [X|Xs2],Xs),inorder(L,Xs1),inorder(R,Xs2).

preorder(nil,[]).
preorder(tree(L,X,R),[X|Xs]):- append(Xs1, Xs2,Xs),preorder(L,Xs1),preorder(R,Xs2).

gentree(Preorder,Inorder,Tree):- inorder(Tree,Inorder),preorder(Tree,Preorder).



    
%### Task 9: Evaluate Expression (without parentheses) ####

% findmuls: goes over the list and replaces the mult expression with its result
findmuls([],[]).

findmuls([X,'*',Y|Rest],L):- 
	T is X*Y, 
	findmuls([T|Rest],L).
	
findmuls([X|Xs],[X|Ys]):-
	findmuls(Xs,Ys).

% iterates over the list and sums all numbers, all mult exprs all handeled above
sumall([],[]).
sumall([X,'+',Y|Rest],L):- 
	T is X+Y, 
	sumall([T|Rest],L).
	
sumall([X|Xs],[X|Ys]):-
	findmuls(Xs,Ys).

% length of a given list
lstlength([], 0).
lstlength([_|T], N) :- 
	lstlength(T, N1), 
	N is N1+1.

evaluate(Exp, Val):- 
	findmuls(Exp,R1),
	sumall(R1,X),
	lstlength(X,1), /*list of a length 1 so it is a value and not an expression*/
	Val is X.


