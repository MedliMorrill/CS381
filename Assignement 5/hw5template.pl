% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_ ).

isFather(X) :- male(X), parent(X,_ ).

% 3. Define a predicate `grandparent/2`.

grandparent(X,Y) :- parent(X,Z),parent(Z ,Y). 

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.


sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.

brother(X,Y) :- sibling(X,Y), male(Y).

sister(X,Y) :- sibling(X,Y), female(Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.

siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).  

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.

aunt(Y,X) :- child(X,Z), sibling(Z,Y), female(Y).
aunt(Y,X) :- child(X,Z), siblingInLaw(Z,Y), female(Y).
uncle(Y,X) :- child(X,Z), sibling(Z,Y), male(Y).
uncle(Y,X) :- child(X,Z), siblingInLaw(Z,Y), male(Y).

% 8. Define the predicate `cousin/2`.

cousin(X,Y) :- aunt(X,Z), parent(Z,Y).
cousin(X,Y) :- uncle(X,Z), parent(Z,Y).

% 9. Define the predicate `ancestor/2`.



% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

expr(N) :- number(N).
expr(S) :- string(S).
expr(B) :- boolean(t), boolean(f).
expr(add(L,R)) :- expr(L), expr(R).
expr(lte(L,R)) :- lte(L), lte(R).
%% expr(Z).
%% expr([]).
%% expr(f).
%% expr(str).

%% cmd(expr(E), X, X).  

cmd(add, [X,Y|Z], [R|Z]) :- cmd(R, Z, S1), R is X+Y.
cmd(lte, [X,Y|Z], [R|Z]) :- cmd(R, Z, S1), R == X<=Y.
ltheq(X,Y).
%% cmd(lte, [X,Y|Z], [R|Z]) :- cmd(R, Z, S1), R is X > Y.
cmd(C, S1, [C|S1]).

%% cmd(if(P1,_),[X|S1],S2) :- prog(P1,S1,S2), X == t.
%% cmd(if(_,P2),[X|S1],S2) :- prog(P2,S1,S2), X == f. 
%% cmd(N, X, Y) :- cmd(E,Num), Y is Num|X.
%% cmd(S, X, Y).
%% cmd(B, X, Y).
%% cmd(add(L, R), X, Y).
%% cmd(lte, X, Y) :- cmd(C, S1, [C|S1])..


% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.


