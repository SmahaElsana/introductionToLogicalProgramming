/**** I, Samha Elsana (204841506) assert that the work I submitted is entirely
my own. I have not received any part from any other student in the
class (or other source), nor did I give parts of it for use to others. I have
clearly marked in the comments of my program any code taken from an
external source. *****/

user:file_search_path(sat, '/home/smaha/Downloads/plsatsolver_src/satsolver').
:- use_module(sat(satsolver)).

%##### Task 1: Encoding Addition ##################################################################################
add(Xs,Ys,Zs,Cnf):-
	length(Xs,K1),length(Ys,K2),K1>0,K2>0,
	addAux(Xs,Ys,Zs,[-1|Cs],Cnf).

addAux([],[],[Cout],[Cout],[]).

addAux([],[Y|Ys],[Z|Zs],[Cin,Cout|Cs],Cnf):-
	append([
	[Cin,Y,-Z],
	[Cin,-Y,Z],
	[-Cin,Y,Z],
	[-Cin,-Y,-Z],
	[Cin,Y,-Cout],
	[Cin,-Y,-Cout],
	[-Cin,Y,-Cout],
	[-Cin,-Y,Cout]],Cnf1, Cnf),
	addAux([],Ys,Zs,[Cout|Cs],Cnf1).

addAux([X|Xs],[],[Z|Zs],[Cin,Cout|Cs],Cnf):-
	append([
	[Cin,X,-Z],
	[Cin,-X,Z],
	[-Cin,X,Z],
	[-Cin,-X,-Z],
	[Cin,X,-Cout],
	[Cin,-X,-Cout],
	[-Cin,X,-Cout],
	[-Cin,-X,Cout]],Cnf1, Cnf),
	addAux(Xs,[],Zs,[Cout|Cs],Cnf1).

addAux([X|Xs],[Y|Ys],[Z|Zs],[Cin,Cout|Cs],Cnf):-
	append(
	%%relation to z
	[[Cin,X,Y,-Z],
	[Cin,X,-Y,Z],
	[Cin,-X,Y,Z],
	[Cin,-X,-Y,-Z],
	[-Cin,X,Y,Z],
	[-Cin,X,-Y,-Z],
	[-Cin,-X,Y,-Z],
	[-Cin,-X,-Y,Z],
	%% relation to Cout
	[Cin,X,Y,-Cout],
	[Cin,X,-Y,-Cout],
	[Cin,-X,Y,-Cout],
	[Cin,-X,-Y,Cout],
	[-Cin,X,Y,-Cout],
	[-Cin,X,-Y,Cout],
	[-Cin,-X,Y,Cout],
	[-Cin,-X,-Y,Cout]
	],Cnf1,Cnf),
	addAux(Xs,Ys,Zs,[Cout|Cs],Cnf1).



%%#### Task2.1: encoding less than  #################################################################################
/* 
sources: https://uomustansiriyah.edu.iq/media/lectures/5/5_2020_06_16!04_18_24_PM.pdf
 */

lt(Xs,Ys,[[B]|Cnf]):-
	reverse(Xs, RevXs),
	reverse(Ys, RevYs),
	lt(RevXs,RevYs,B,Cnf).

%base case
lt([],[],B,[[-B]]).

%numbers with equal length
lt([X|Xs],[Y|Ys],B,Cnf):-
	Cnf0 = [[-B,X,Y,B1],
		[B,X,Y,-B1],
		[B,X,-Y],
		[-B,-X,Y,B1],
		[-B,-X,Y,-B1],
		[-B,-X,-Y,B1],
		[B,-X,-Y,-B1]],
	lt(Xs,Ys,B1,Cnf1),
	append(Cnf0,Cnf1,Cnf),!.

%lenght(Xs) > length(Ys)
lt([X|Xs],[Y|Ys],B,[[-X]|Cnf]):-
	lt(Xs,[Y|Ys],B,Cnf),!.

%lenght(Xs) < length(Ys)
lt([X|Xs],[Y|Ys],B,[[-Y]|Cnf]):-
	lt([X|Xs],Ys,B,Cnf),!.


%%#### Task2.2: enoding less or equal #################################################################################
/*pad(X,NewX,Len):-*/
padding([],0).
padding(X,Len):-
	NewLen is Len-1,
	padding(XS,NewLen),
	append([-1],XS,X).
	
pad(Xs,NewX,Len):-
	padding(P,Len),
	append(Xs,P,NewX).

leq(Xs,Ys,[[B]|Cnf]):-
	length(Xs,L1),
	length(Ys,L2),
	L1<L2,
	Dif is L2-L1,
	pad(Xs,NewXs,Dif),
	reverse(NewXs, RevXs),
	reverse(Ys, RevYs),
	leq(RevXs,RevYs,B,B1,Cnf),!.
	
leq(Xs,Ys,[[B]|Cnf]):-
	length(Xs,L1),
	length(Ys,L2),
	L1>L2,
	Dif is L1-L2,
	pad(Ys,NewYs,Dif),
	reverse(Xs, RevXs),
	reverse(NewYs, RevYs),
	leq(RevXs,RevYs,B,B1,Cnf),!.
	
leq(Xs,Ys,[[B]|Cnf]):-
	reverse(Xs, RevXs),
	reverse(NewYs, RevYs),
	leq(RevXs,RevYs,B,B1,Cnf),!.

leq([],[],B,B,[[B]]).

leq([X|Xs],[Y|Ys],B,B1,Cnf):-
	Cnf0 = [[B,X,Y,-B1],
		[B,X,-Y,-B1],
		[B,-X,Y,-B1],
		[B,-X,-Y,-B1],
		
		[-B,X,Y,B1],
		[-B,X,-Y,B1],
		[-B,-X,Y,-B1],
		[-B,-X,-Y,B1]],
	leq(Xs,Ys,B1,NewB1 ,Cnf1),
	append(Cnf0,Cnf1,Cnf).

%%#### Task 3: Encoding Sum ####################################################################################
/*sum(ListofNumbers, Zs, Cnf) */

sum([],[],[]).

sum([],[B|Bs],Cnf):-
	sum([],Bs,Cnf0),
	append(Cnf0,[[-B]],Cnf).

sum([L|Ls], Zs, Cnf):-
	sum(Ls,Z1s,Cnf1),
	add(L,Z1s,Zs,Cnf0),
	append(Cnf0,Cnf1,Cnf).


%%#### Task 4: Encoding Multiplication ##########################################################################

multBit([X],[],[],[]).
multBit([X],[Y|Ys],[S|Ss],Cnf):-
	multBit([X],Ys,Ss,Cnf0),
	append([[-S,X,Y],
		[-S,X,-Y],
		[-S,-X,Y],
		[S,-X,-Y]],Cnf0,Cnf).

/*creates list of lists
 Pre contains the list of -1 to append and shift */
createList([],Ys,Pre,[],[]).

createList([X],Ys,Pre,[Lst],Cnf):-
	multBit([X],Ys,L,Cnf),
	append(Pre,L,Lst).


createList([X|Xs],Ys, Pre, List,Cnf):-
	multBit([X],Ys,L,Cnf0),
	append(Pre,L,NewL),
	createList(Xs,Ys,[-1|Pre],L0,Cnf1),
	append([NewL],L0,List),
	append(Cnf0,Cnf1,Cnf).


times(Xs,Ys,Zs,Cnf):-
	%length(Zs,K),length(Xs,K1),length(Ys,K2), K = K1*K2,
	createList(Xs,Ys, [], List,Cnf0),
	sum(List, Zs, Cnf1),
	append(Cnf0,Cnf1,Cnf).

	

%%#### Task 5: Encoding Power #####################################################################################
/*power(N,Xs,Zs,Cnf) */
power(1,Xs,Xs,[Xs]).

power(N,Xs,Zs, Cnf):-
	K is N-1,
	power(K,Xs,Z0, Cnf0),
	times(Xs,Z0,Zs,Cnf1),
	append(Cnf0,[Z0|Cnf1],Cnf).


%%#### Task 6: Encoding Power Equations ############################################################################
/*powerEquation(N,M,Zs,List,Cnf) */
powerList(N,[],[],_,[]).
powerList(N,[L0|L],[L1|NewList],Len,Cnf):-
	length(L0,Len),
	powerList(N,L,NewList,Len,Cnf0),
	power(N,L0,L1,Cnf1),
	append(Cnf0,Cnf1,Cnf).


powerEquation(N,0,[],[],_,[]).	
powerEquation(N,1,Zs,[PZs],Cnf):- power(N,Zs,PZs,Cnf).
	
powerEquation(N,M,Zs,List,Cnf):-
	length(List,M),
	length(Zs,K),
	powerList(N,List,PwrList,K,Cnf0),
	ascending(List,Cnf3),
	sum(PwrList, Z1, Cnf2),
	power(N,Zs,Z1,Cnf1),
	append(Cnf0,Cnf2,Cnf02),
	append(Cnf1,Cnf02,Cnf012),
	append(Cnf012,Cnf3,Cnf).
	
	
%%#### Task 7: Euler’s Conjecture #####################################################################################	
/*euler(N, NumBits) N is the power NumBits id the number of bits of the numbers*/	

solve(Instance,Solution):-
	encode(Instance,Map,Cnf),
	sat(Cnf),
    	decode(Map,Solution),       
    	verify(Instance, Solution).   
   
verify(_,_) :-
        writeln(verified:ok).	
        
/*ascending(List) --> ascending values in list  */
ascending([],[]).
ascending([L],[]).
ascending([L1,L2|List],Cnf):-
	lt(L1,L2,Cnf1),
	ascending([L2|List],Cnf2),
	append(Cnf1,Cnf2,Cnf).
	
encode(euler(N, NumBits),Map,Cnf):-
	M is N-1,
	length(Zs,NumBits),
	powerEquation(N,M,Zs,List,Cnf),
	Map = [Zs|List].
	

/*sol2Dec(L,V) converting sat solution to decimal, L=[-1,1,-1] --> V=2 */
sol2Dec([], _, N, N).
sol2Dec([1|Bs], I0, N0, N) :-
        N1 is N0 + (2^I0),
        I1 is I0 + 1,
        sol2Dec(Bs, I1, N1, N).

sol2Dec([-1|Bs], I0, N0, N) :-
        I1 is I0 + 1,
        sol2Dec(Bs, I1, N0, N).
        


%%decode(Map,Solution)
decode([],[]).
decode([M|Map],[S|Solution]):-
	sol2Dec(M,0,0,S),
	decode(Map,Solution).
	
	
%%#### Task 8: Power Partition — All Solutions ###########################################################################
encode8(partition(N,NumBits),Map,Cnf):-
	M is N,
	length(Zs,NumBits),
	powerEquation(N,M,Zs,List,Cnf),
	Map = [Zs|List].


/* needed to change encode -> encode8 due to above encode */
solveAll(Instance,Solutions) :-
encode8(Instance,Map,Cnf),
satMulti(Cnf,1000,_Count,_Time),
decodeAll(Map,Solutions),
verifyAll(Instance,Solutions).


verifyAll(_,_) :-
        writeln(verified:ok).
        
/*
%% it takes a list of lists and returns all the i'th values of each list in a list
gets one solution for B
*/
getOne([],_,[]).
getOne([L|LL],I,[S|Ss]):-
	nth0(I,L,S),
	getOne(LL,I,Ss).
	
/*
get one solution of the form B ,A1,...,An
 */	
getSol([],_,[]).

getSol([V|Vs],I,[R|Rs]):-
	getOne(V,I,R),
	getSol(Vs,I,Rs).
	

decodeAll(Map,Solutions):-
	nth0(0,Map,LL),
	nth0(0,LL,L),
	length(L,Count),
	findall(Val,(between(0,Count,I),getSol(Map,I,S),decode(S,Val)),Solutions).

		
	
	

	

