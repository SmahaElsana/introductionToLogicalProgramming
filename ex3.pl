/**** I, Smaha Elsana (204841506) assert that the work I submitted is entirely my
own. I have not received any part from any other student in the class (or other
source), nor did I give parts of it for use to others. I have clearly marked in
the comments of my program any code taken from an external source. *****/

user:file_search_path(sat, '/users/studs/bsc/2019/samha/bee20170615/satsolver').
user:file_search_path(bee, '/users/studs/bsc/2019/samha/bee20170615/beeCompiler').
:- use_module(sat(satsolver)).
:- use_module(bee(bCompiler)).
:- use_module(bee('../bApplications/auxs/auxRunExpr'),[runExpr/5]).
:- use_module(bee('../bApplications/auxs/auxRunExprAll'),[runExprAll/5]).
%verifyIsntance(Instance):-

/* Task 1: kakuroVerify-----------------------------------------------------------------------------------------*/
/*different values in list & in range 1-9*/
legal_block([]).
legal_block([H|T]):-
   	\+member(H,T),
	member(H,[1,2,3,4,5,6,7,8,9]),
   	legal_block(T).

diff_list([]).
diff_list([H|T]):-
   	\+member(H,T),
   	diff_list(T).

kakuroVerify([],[]).
kakuroVerify([Icluesum = Iblock|Instance], [Scluesum = Sblock |Solution]):-
	Icluesum =:= Scluesum,
	%length(Iblock,L1), length(Sblock,L2), L1 =:= L2,
	is_list(Iblock), is_list(Sblock),
	same_length(Iblock,Sblock),
	/*%sum of Sblock equals Scluesum*/
	sum_list(Sblock,Sum), Sum =:= Scluesum,
	legal_block(Sblock),
	kakuroVerify(Instance, Solution).
	
	
	
/* Task 2: kakuroEncode -----------------------------------------------------------------------------------------*/
/*kakuroEncode(Instance,Map,Constraints)*/
declareInts([],[]).
declareInts([X|IntList],[new_int(X,1,9)|Constraints]):-
	declareInts(IntList,Constraints).
	
getBlocks([],[]).
getBlocks([_=Block|Instance],Is):-
	append(Block,SubIs,Is),
	getBlocks(Instance,SubIs).

/*sumCon(Instance,Constraints)*/
sumCon([],[]).
sumCon([Cluesum=Block|Instance],Con):-
	C1 = int_array_allDiff(Block),
	C2 = int_array_sum_eq(Block, Cluesum),
	append([C1,C2],C,Con),
	sumCon(Instance,C).


kakuroEncode(Instance,Map,Constraints):-
	getBlocks(Instance,List),
	%sort(List,Newlist),
	term_variables(List,NewList),
	declareInts(NewList,C1),
	sumCon(Instance,C2),
	append(C1,C2,Constraints),
	Map = map(Instance).
	
/* Task 3: kakuroDecode --------------------------------------------------------------------------------------------*/
/*kakuroDecode(Map,Solution)*/
kakuroDecode(map([]),[]).
kakuroDecode(map([Icluesum=Iblock|Instance]),[Scluesum=Sblock|Solution]):-
	bDecode:decodeInt(Icluesum,Scluesum),
	bDecode:decodeIntArray(Iblock,Sblock),
	kakuroDecode(map(Instance),Solution).

/* Task 4: kakuroSolve ---------------------------------------------------------------------------------------------*/
/* kakuroSolve(Instance,Solution)*/
%%the commented kakuroSolve prints the solution as shown in class
/*
kakuroSolve(Instance,Solution):-
	kakuroEncode(Instance,Map,Constraints),
	bCompile(Constraints,Cnf),
	sat(Cnf),
	kakuroDecode(Map,Solution),
	(kakuroVerify(Instance,Solution))-> writeln(Solution:ok);writeln(Solution:wrong).
*/

kakuroSolve(Instance,Solution):-
	kakuroEncode(Instance,Map,Constraints),
	bCompile(Constraints,Cnf),
	sat(Cnf),
	kakuroDecode(Map,Solution),
	kakuroVerify(Instance,Solution).

/********************************************************* KILLER SUDOKU ********************************************************************************/

/* Task 5: Killer Sudoku Verifier -----------------------------------------------------------------------------------*/
/*verify_killer(Instance,Solution)*/

/* Task 5: Killer Sudoku Verifier */

check_row(I,Solution):-
	findall(Val,(member(X,Solution),X= (cell(I,J)=Val) ,between(1,9,J)),Row),
	legal_block(Row).

check_col(J,Solution):-
	findall(Val,(member(X,Solution),X= (cell(I,J)=Val) ,between(1,9,I)),Col),
	legal_block(Col).

check_box(I,J,Solution):-
	findall(Val,(member(X,Solution),X= (cell(I1,J1)=Val) ,between(1,9,I1),between(1,9,J1),J1-J=<2,I1-I=<2,0=<J1-J,0=<I1-I),Box),
	%diff_list(Box).
	legal_block(Box).

check_knight(Cur,I,J,Solution):-
/*cell(I,J)=Cur*/
	findall(Val,(member(X,Solution),X= (cell(I1,J1)=Val),
	abs(J1-J)=<2,
	abs(I1-I)=<2,
	I1=\=I,J1=\=J,
	(abs(J1-J)+abs(I1-I))=:=3
	),Knight),
	\+member(Cur,Knight).
	
check_king(Cur,I,J,Solution):-
/*cell(I,J)=Cur*/
	findall(Val,(member(X,Solution),X= (cell(I1,J1)=Val),
	abs(J1-J)=<1,
	abs(I1-I)=<1,
	((I1=\=I,J1==J);(I1==I,J1=\=J);(I1=\=I,J1=\=J))
	),King),
	\+member(Cur,King).
	
check_adjacent(Cur,I,J,Solution):-
	findall(Val,(member(X,Solution),X= (cell(I1,J1)=Val),
	abs(J1-J)=<1,
	abs(I1-I)=<1,
	((I1=\=I,J1==J);(I1==I,J1=\=J))
	%,abs(Cur-Val)>=2
	),King),
	forall(member(X,King),abs(Cur-X)>=2),
	\+member(Cur,King).

check_rows(Solution):-
	forall(member(I,[1,2,3,4,5,6,7,8,9]),check_row(I,Solution)).
check_cols(Solution):-
	forall(member(J,[1,2,3,4,5,6,7,8,9]),check_row(J,Solution)).

check_boxes(Solution):-
	forall((member(I,[1,4,7]),member(J,[1,4,7])),check_box(I,J,Solution)).

check_knights(Solution):-
	forall((member(X,Solution),X= (cell(I,J)=Val)),check_knight(Val,I,J,Solution)).

check_kings(Solution):-
	forall((member(X,Solution),X= (cell(I,J)=Val)),check_knight(Val,I,J,Solution)).

check_adjacents(Solution):-
	forall((member(X,Solution),X= (cell(I,J)=Val)),check_adjacent(Val,I,J,Solution)).
	
/*verify_killer(Instance,Solution)*/
/*the commented verify prints the solution as shown in class*/
/*
verify_killer(_,Solution):-
	(check_rows(Solution),
	check_cols(Solution),
	check_boxes(Solution),
	check_knights(Solution),
	check_kings(Solution),
	check_adjacents(Solution)) -> writeln(Solution:ok); writeln(Solution:wrong).
*/

verify_killer(_,Solution):-
	check_rows(Solution),
	check_cols(Solution),
	check_boxes(Solution),
	check_knights(Solution),
	check_kings(Solution),
	check_adjacents(Solution).	
	
/* Task 6: Killer Sudoku Encoder --------------------------------------------------------------------------------------------------*/
/*encode_killer(Instance, Map, Constraints)*/

get_row(_,[],[]).
get_row(I,[cell(IR,_)=Val|Cells],Row):-
	(I =:= IR) -> (append([Val],SubR,Row),get_row(I,Cells,SubR));(get_row(I,Cells,Row)).


get_col(_,[],[]).
get_col(I,[cell(_,IC)=Val|Cells],Col):-
	(I =:= IC) -> (append([Val],SubC,Col),get_col(I,Cells,SubC));(get_col(I,Cells,Col)).

get_box(_,_,[],[]).
get_box(I,J,[cell(IR,IC)=Val|Cells],Box):-
	(IC-J=<2,IR-I=<2,0=<IC-J,0=<IR-I) -> (append([Val],SubB,Box),get_box(I,J,Cells,SubB));(get_box(I,J,Cells,Box)).
/** above rules give lists of wanted cells**/

/**below rules calculates constraints**/
/*get_knight(I,J,Cur,Cells,Constraints)*/
get_knight(_,_,_,[],[]).
get_knight(I,J,(cell(I,J)=X),[cell(IR,IC)=Val|Cells],Constraints):-
	(abs(IC-J)=<2,
	abs(IR-I)=<2,
	IR=\=I,IC=\=J,
	(abs(IC-J)+abs(IR-I))=:=3
	) -> (append([int_array_allDiff([X,Val])],SubK,Constraints),get_knight(I,J,(cell(I,J)=X),Cells,SubK));(get_knight(I,J,(cell(I,J)=X),Cells,Constraints)).

get_kings(_,_,_,[],[]).
get_kings(I,J,(cell(I,J)=X),[cell(IR,IC)=Val|Cells],Constraints):-
	(abs(IC-J)=<1,
	abs(IR-I)=<1,
	((IR=\=I,IC==J);(IR==I,IC=\=J);(IR=\=I,IC=\=J))
	) -> (append([int_array_allDiff([X,Val])],SubK,Constraints),get_kings(I,J,(cell(I,J)=X),Cells,SubK));(get_kings(I,J,(cell(I,J)=X),Cells,Constraints)).

get_adjacent(_,_,_,[],[]).
get_adjacent(I,J,(cell(I,J)=X),[cell(IR,IC)=Val|Cells],Constraints):-
	(abs(IC-J)=<1,
	abs(IR-I)=<1,
	((IR=\=I,IC==J);(IR==I,IC=\=J))
	) -> (append([new_int(C,[-8,-7,-6,-5,-4,-3,-2,2,3,4,5,6,7,8]),int_array_sum_eq([X,C],Val)],SubK,Constraints),get_adjacent(I,J,(cell(I,J)=X),Cells,SubK));(get_adjacent(I,J,(cell(I,J)=X),Cells,Constraints)).	
	
value_constraint((cell(I,J)=X),Instance,Constraint):-
	((member((cell(I,J)=Val),Instance)) -> (Constraint=new_int(X,Val,Val)); (Constraint=new_int(X,1,9))).

encode_values([],_,[]).
encode_values([(cell(I,J)=Val)|Cells],Instance,Constraints):-
	value_constraint((cell(I,J)=Val),Instance,C1),
	encode_values(Cells,Instance,C2),
	append([C1],C2,Constraints).

/*encode_rows(I,Cells,Constraints)*/	
encode_rows(9,Cells,Constraints):-
	get_row(9,Cells,Row),
	Constraints = [int_array_allDiff(Row)].
encode_rows(I,Cells,Constraints):-
	I=<9,
	get_row(I,Cells,Row),
	NextI is I+1,
	C1 = [int_array_allDiff(Row)],
	encode_rows(NextI,Cells,C2),
	append(C1,C2,Constraints).


encode_cols(9,Cells,Constraints):-
	get_row(9,Cells,Col),
	Constraints = [int_array_allDiff(Col)].
encode_cols(I,Cells,Constraints):-
	I=<9,
	get_col(I,Cells,Col),
	NextI is I+1,
	C1 = [int_array_allDiff(Col)],
	encode_cols(NextI,Cells,C2),
	append(C1,C2,Constraints).

encode_boxes([],[]).	
encode_boxes([cell(I,J)=Val|Cells],Constraints):-
	(mod(I,3)=:=1, mod(J,3) =:=1) -> (get_box(I,J,[cell(I,J)=Val|Cells],Box),
	C1 = [int_array_allDiff(Box)],
	encode_boxes(Cells,C2),
	append(C1,C2,Constraints));
	(encode_boxes(Cells,Constraints)).
	

encode_knights([],[]).
encode_knights([cell(I,J)=Val|Cells],Constraints):-
	get_knight(I,J,(cell(I,J)=Val),[cell(I,J)=Val|Cells],C1),
	encode_knights(Cells,C2),
	append(C1,C2,Constraints).

encode_kings([],[]).
encode_kings([cell(I,J)=Val|Cells],Constraints):-
	get_kings(I,J,(cell(I,J)=Val),[cell(I,J)=Val|Cells],C1),
	encode_kings(Cells,C2),
	append(C1,C2,Constraints).

encode_adjacents([],[]).
encode_adjacents([cell(I,J)=Val|Cells],Constraints):-
	get_adjacent(I,J,(cell(I,J)=Val),[cell(I,J)=Val|Cells],C1),
	encode_adjacents(Cells,C2),
	append(C1,C2,Constraints).	

encode_killer(killer(Instance), Map, Constraints):-
	findall(cell(I,J)=_,(between(1,9,I),between(1,9,J)),Cells),
	encode_values(Cells,Instance,C1),
	encode_rows(1,Cells,C2),
	encode_cols(1,Cells,C3),
	encode_knights(Cells,C4),
	encode_kings(Cells,C5),
	encode_boxes(Cells,C6),
	encode_adjacents(Cells,C7),
	append([C1,C2,C3,C4,C5,C6,C7],Constraints),
	Map=map(Cells).
	
	
/* Task 7: Killer Sudoku Decoder ----------------------------------------------------------------------------------*/
decode_killer(map(Cells),Solution):-
	findall((cell(I,J)=IntVal),(member((cell(I,J)=Val),Cells),bDecode:decodeInt(Val,IntVal)),Solution).
/*	
solve_killer(Instance, Solution) :-
	runExpr(Instance, Solution, encode_killer, decode_killer, verify_killer).
*/	
solve_killer(Instance,Solution):-
	encode_killer(Instance,Map,Constraints),
	bCompile(Constraints,Cnf),
	sat(Cnf),
	decode_killer(Map,Solution),
	verify_killer(Instance,Solution).	
/*Below: using the framework - runExpr */
solve_framework(Instance,Solution):-
    writef('%w,',[Instance]),flush_output,
    runExpr(Instance,Solution,
            encode_killer,
            decode_killer,
            verify_killer).	
	
/* Task 8: Legal Killer Sudoku Instance ---------------------------------------------------------------------------*/
extract_ints([],[]).
extract_ints([cell(I,J)=Val|Cells],RelevantInts):-
	extract_ints(Cells,RI),
	append([Val],RI,RelevantInts).

encode_killer(Instance,Map,(RelevantBools,RelevantInts),Constraints) :-
    encode_killer(Instance, Map, Constraints),
    Map = map(Cells),
    RelevantBools = [], 
    extract_ints(Cells,RelevantInts).

all_killer(Instance, Solutions):-
	writef('%w,',[Instance]),flush_output,
    runExprAll(Instance,Solutions,
            encode_killer,
            decode_killer,
            verify_killer).


