male(fritz).
male(erich).
male(conrad).
male(dennis).
male(berti).
male(albert).

female(X) :- not(male(X)).

parent(berti, albert).
parent(beate, albert).

parent(dennis, beate).
parent(dora, beate).

parent(conrad, berti).
parent(claudia, berti).

parent(erich, conrad).
parent(eva, conrad).

parent(fritz, eva).

father(X, Y) :- male(X), parent(X, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

female_ancestor(X, Y) :- ancestor(X, Y), female(X).
