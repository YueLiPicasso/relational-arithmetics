% binary arithmetic

zero([]).
pos([_|_]).
gt1([_,_|_]).

genb([]).
genb([1|Bs]):-genb(Bs).
genb([0|Bs]):-pos(Bs),genb(Bs).

% less then in length
lessl([],[_|_]).
lessl([_|X],[_|Y]):-lessl(X,Y).

% full adder for a pair of digits
% (c-in, bit-1, bit-2, sum-bit, c-out)
full_adder(0,0,0,0,0).
full_adder(0,0,1,1,0).
full_adder(0,1,0,1,0).
full_adder(0,1,1,0,1).
full_adder(1,0,0,1,0).
full_adder(1,0,1,0,1).
full_adder(1,1,0,0,1).
full_adder(1,1,1,1,1).

% different versions of adders

% adder1(c-in,num-1,num-2,sum)

adder1(0,A,[],A).
adder1(0,[],B,B):-pos(B).
adder1(1,A,[],R):-adder1(0,A,[1],R).
adder1(1,[],B,R):-pos(B),adder1(0,[1],B,R).
adder1(Cin,[A1|A2],[B1|B2],[R1|R2]):-
	full_adder(Cin, A1,B1,R1,Cout),
	adder1(Cout,A2,B2,R2).

% ?- adder1(0,X,[1],[1]).
% X = [] ;
% X = [0] ;
% false.
% so for  ?- adder1(0,[1],X,[1]).

% the above undesired results occur becuase:
% adder1(0,_4666,[1],[1]) unifies both
% clause 2 and clause 5
% adder1(0,_4666,[1],[1]) unifies both
% clause 2 and clause 5




% adder(c-in,num-1,num-2,sum)

adder(0,A,[],A).
adder(0,[],B,B):-pos(B).
adder(1,A,[],R):-adder(0,A,[1],R).
adder(1,[],B,R):-pos(B),adder(0,[1],B,R).

adder(Cin,[1],[1],R):-R = [R1|R2],
	full_adder(Cin,1,1,R1,R2).
adder(Cin,[1],[B1|B2],[R1|R2]):-pos(B2),pos(R2),
	full_adder(Cin,1,B1,R1,Cout),
	adder(Cout,[],B2,R2).
adder(Cin,A,[1],R):-gt1(A),gt1(R),adder(Cin,[1],A,R).

adder(Cin,[A1|A2],[B1|B2],[R1|R2]):-
    pos(A2),pos(B2),pos(R2),
	full_adder(Cin, A1,B1,R1,Cout),
	adder(Cout,A2,B2,R2).

