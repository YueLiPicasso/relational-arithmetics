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

% adder1 has the following undersired behaviours
% 
% ?- adder1(0,X,[0,0,1],[0,0,1]).
% X = [] ;
% X = [0] ;
% X = [0,0] ;
% X = [0,0,0] ;
% false.
%
% The above undesired results occur becuase:
% adder1(0,_4666,[0,0,1],[0,0,1]) unifies both
% clause 2 and clause 5
%
% adder1 is correct in the sense that if all
% padded zeros are removed, the answer is correct. 
% 
% The minimum example of the problem is
% ?- adder1(0,X,[1],[1]).
% X = [] ;
% X = [0] ;
% false.
%
% Although I don't know how to solve this problem in
% general, trying to eliminate at least the minimum
% of the problem naturally requires:

adder2(0,A,[],A).
adder2(0,[],B,B):-pos(B).
adder2(1,A,[],R):-adder2(0,A,[1],R).
adder2(1,[],B,R):-pos(B),adder2(0,[1],B,R).
adder2(Cin,[A1|A2],[B1|B2],[R1|R2]):-
	pos(A2),pos(B2),
	full_adder(Cin, A1,B1,R1,Cout),
	adder2(Cout,A2,B2,R2).

% adder2 seems to solve the problems of adder1
% in general. However, new problems emerge, such as
% ?- adder2(0,X,[1],[1,0,1]).
% false.
% and even
% ?- adder2(0,[1,0,1], [1,1,1],X).
% false.

% This reminds us to add (non-overlapping) cases 
% to handle addition of [1], which leads to:


adder3(0,A,[],A).                 % clause 1
adder3(0,[],B,B):-pos(B).
adder3(1,A,[],R):-adder3(0,A,[1],R).
adder3(1,[],B,R):-pos(B),adder3(0,[1],B,R).

adder3(Cin,[1],[B1|B2],[R1|R2]):- % clause 5
	full_adder(Cin,1,B1,R1,Cout),
	adder3(Cout,[],B2,R2).
adder3(Cin,A,[1],R):-             
	gt1(A),adder3(Cin,[1],A,R).

adder3(Cin,[A1|A2],[B1|B2],[R1|R2]):-
	pos(A2),pos(B2),
	full_adder(Cin, A1,B1,R1,Cout),
	adder3(Cout,A2,B2,R2).

% All earlier examples of problems are fixed by adder3,
%
% But see this:
% ?- adder3(0,[1],X,[1]).
% X = [] ;
% X = [0] ;
% false.
%
% An extra padded zero !
% This is because adder3(0,[1],X,[1]) unifies with 
% clause 1 then clause 5. To prevent, 
% in clause 5 we want R2 to be positive:

adder4(0,A,[],A).               
adder4(0,[],B,B):-pos(B).
adder4(1,A,[],R):-adder4(0,A,[1],R).
adder4(1,[],B,R):-pos(B),adder4(0,[1],B,R).

adder4(Cin,[1],[B1|B2],[R1|R2]):- % clause 5
	pos(R2),
	full_adder(Cin,1,B1,R1,Cout),
	adder4(Cout,[],B2,R2).
adder4(Cin,A,[1],R):-             
	gt1(A),adder4(Cin,[1],A,R).

adder4(Cin,[A1|A2],[B1|B2],[R1|R2]):-
	pos(A2),pos(B2),
	full_adder(Cin, A1,B1,R1,Cout),
	adder4(Cout,A2,B2,R2).

% How about this:
% ?- adder4(0,X,Y,[0,0,1]).
% ...<valid answers omitted>...
% X = [1, 1],
% Y = [1, 0] ;
% false.

% Below is the decidable version given by the reference paper.
% We can see that from adder1 to adder4 we are approaching
% decidability. 

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

% note that, 
% ?- adder(0,[0],[],Y).
% Y = [0] ;
% false.
% So uur expectation on adder should be:
% given valid input, it returns valid output,
% with padding zeros removed. 
