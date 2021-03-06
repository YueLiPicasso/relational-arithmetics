% unary natural number arithmetic
% SWI-Prolog 8.0.3

% 0 -- []
% n -- [u, u|...] n copies of u

add([],X,X).
add([u|X],Y,[u|Z]):-add(X,Y,Z).

mul1([],_,[]).
mul1([u|X],Y,Z):-mul1(X,Y,W),add(W,Y,Z).

% swap add and recursive call of mul wrt. mul1
mul2([],_,[]).
mul2([u|X],Y,Z):-add(W,Y,Z),mul2(X,Y,W).

% swap args of add wrt. mul2
mul3([],_,[]).
mul3([u|X],Y,Z):-add(Y,W,Z),mul3(X,Y,W).

% zero multiplicands are treated separately wrt. mul3
mul([],_,[]).
mul([u|_],[],[]).
mul([u|X],[u|Y],Z):-add([u|Y],W,Z),mul(X,[u|Y],W).

% alternative path of modification

% swap args of mul wrt. mul3
mult([],_,[]).
mult([u|X],Y,Z):-add(Y,W,Z),mult(Y,X,W).

% the output of mul(X,Y,Z) is exactly the same as mult(X,Y,Z)


% generate half of the answer set under Prolog's serach 
% strategy

less([],[_|_]).
less([u|X],[u|Y]):- less(X,Y).

semimul([],_,[]).
semimal([u|_],[],[]).
semimul([u|X],[u|Y],Z):-less(X,Z),
						less(Y,[u|X]),
						mul([u|X],[u|Y],Z).

