:- module(hdml_parser, [parse/3]).

parse(_Path, Codes, Program) :-
  lekser(Codes, Tokens),
  program(Program, Tokens, []).

% lekser
lekser(Codes, Tokens):-
  string_to_list(Codes, Numbers),
  maplist(char_code, Letters, Numbers),
  tokener(Letters, Tokens).

% "tokener"
tokener([], []):- !.
tokener(Letters, Tok):-
  one_token(Letters, Letts, Token),
  if_add(Token, Tokens, Tok),
  tokener(Letts, Tokens).

if_add([], Tokens, Tokens):- !.
if_add(Token, Tokens, [Token|Tokens]).

% Tokeny
% komentarze i nadmiarowe biale znaki wyrzucam jako []
one_token(Letters, Letts, Token):-
  word(Letters, [], Letts, RWord),
  reverse(RWord, Word),
  which_token(Word, Token).

% komentarze
word(['*',')'|Letters], [com|LTok], Letts, LTok1):-
  comment(LTok), !,
  word(Letters, LTok, Letts, LTok1).
word(['*',')'|Letts], [com], Letts, []):- !.
word(['(','*'|Letters], LTok, Letts, LTok1):-
  comment([com|LTok]), !,
  word(Letters, [com|LTok], Letts, LTok1).
word([_|Letters], LTok, Letts, LTok1):-
  comment(LTok), !, word(Letters, LTok, Letts, LTok1).
word(['(','*'|Letts], LTok, ['(','*'|Letts], LTok):- !.
% slowa itp.
word([L|Letters], LTok, Letts, LTok1):-
  L @>= 'a', L @=< 'z', !,
  word(Letters, [L|LTok], Letts, LTok1);
  L @>= 'A', L @=< 'Z', !,
  word(Letters, [L|LTok], Letts, LTok1);
  L @>= '0', L @=<'9', !,
  word(Letters, [L|LTok], Letts, LTok1);
  L = '_', !, word(Letters, [L|LTok], Letts, LTok1);
  L = '\'', !, word(Letters, [L|LTok], Letts, LTok1).
% znaki biale
word([L|Letts], LTok, Letts, LTok):-
  char_code(L, N), N >= 9, N =< 13, !;
  char_code(L, N), N = 32, !.
% operatory i znaki przestankowe
word([L1,L2|Letts], [], Letts, [L2,L1]):-
  L1 = '.', L2 = '.', !; L1 = '<', L2 = '>', !;
  L1 = '<', L2 = '=', !; L1 = '>', L2 = '=', !.
word([L1,L2|Letts], LTok, [L1,L2|Letts], LTok):-
  L1 = '.', L2 = '.', !; L1 = '<', L2 = '>', !;
  L1 = '<', L2 = '=', !; L1 = '>', L2 = '=', !.
word([L|Letts], [], Letts, [L]):-
  L = '(', !; L = ')', !; L = '[', !; L = ']', !;
  L = ',', !; L = '=', !; L = '<', !; L = '>', !;
  L = '^', !; L = '|', !; L = '+', !; L = '-', !;
  L = '&', !; L = '*', !; L = '/', !; L = '%', !;
  L = '@', !; L = '#', !; L = '~', !.
word([L|Letts], LTok, [L|Letts], LTok):-
  L = '('; L = ')'; L = '['; L = ']';
  L = ','; L = '='; L = '<'; L = '>';
  L = '^'; L = '|'; L = '+'; L = '-';
  L = '&'; L = '*'; L = '/'; L = '%';
  L = '@'; L = '#'; L = '~'.
% koniec
word([], LTok, [], LTok).

comment([com]):- !.
comment([com|T]):- comment(T).

which_token([],[]).
which_token([com|_],[]):- !, false.

which_token(['d','e','f'],sk(def)):- !.
which_token(['e','l','s','e'],sk(else)):- !.
which_token(['i','f'],sk(if)):- !.
which_token(['i','n'],sk(in)):- !.
which_token(['l','e','t'],sk(let)):- !.
which_token(['t','h','e','n'],sk(then)):- !.
which_token(['_'],sk('_')):- !.

which_token(Nchar,li(N)):-
  is_number(Nchar), !,
  maplist(char_code,Nchar,Num),
  concat_number(Num, N).

which_token([L|Let],id(Id)):-
  L = '_', is_identifier(Let), !,
  string_to_list(I,[L|Let]), atom_codes(Id,I);
  L @>= 'a', L @=< 'z', is_identifier(Let), !,
  string_to_list(I,[L|Let]), atom_codes(Id,I);
  L @>= 'A', L @=< 'Z', is_identifier(Let), !,
  string_to_list(I,[L|Let]), atom_codes(Id,I).

which_token(Op, opb5(A)):-
  atom_codes(A, Op), is_opb5(A), !.
which_token(Op, opb4(A)):-
  atom_codes(A, Op), is_opb4(A), !.
which_token(Op, opb3(A)):-
  atom_codes(A, Op), is_opb3(A), !.
which_token(Op, opb2(A)):-
  atom_codes(A, Op), is_opb2(A), !.
which_token(Op, opb1(A)):-
  atom_codes(A, Op), is_opb1(A), !.
which_token(Op, opu(A)):-
  atom_codes(A, Op), is_opu(A), !.
which_token(Op, opn('(')):-
  atom_codes('(', Op), !.
which_token(Op, opn(')')):-
  atom_codes(')', Op), !.
which_token(Op, opn('[')):-
  atom_codes('[', Op), !.
which_token(Op, opn(']')):-
  atom_codes(']', Op), !.
which_token(Op, opkk):-
  atom_codes('..', Op).

is_number([N]):-
  N @>= '0', N @=< '9', !.
is_number([N|Num]):-
  N @>= '0', N @=< '9', !, is_number(Num).

is_identifier([]).
is_identifier([L|Let]):-
  L = '_', !, is_identifier(Let);
  L = '\'', !, is_identifier(Let);
  L @>= 'a', L @=< 'z', !, is_identifier(Let);
  L @>= 'A', L @=< 'Z', !, is_identifier(Let);
  L @>= '0', L @=< '9', !, is_identifier(Let).

is_opb5(X):-
  X = '&'; X = '*'; X = '/'; X = '%'.
is_opb4(X):-
  X = '|'; X = '^'; X = '+'; X = '-'.
is_opb3('@').
is_opb2(X):-
  X = '='; X = '<>'; X = '<'; X = '>'; X = '<='; X = '>='.
is_opb1(',').

is_opu(X):-
  X = '-'; X = '#'; X = '~'.

concat_number(D, N):-
    reverse(D, X), concat_number2(X, N).
concat_number2([X], Y):-
    Y is X - 48, Y >= 0, Y =< 9.
concat_number2([H|D], N):-
    H1 is H - 48, H1 >= 0, H1 =< 9,
    concat_number2(D, N2), N is N2 * 10 + H1.

%gramatyka

program(X) --> definicje(X).

definicje([]) --> [].
definicje([D|X]) --> definicja(D), definicje(X).

definicja(def(Name, P, E)) -->
  [sk(def), id(Name), opn('(')], wzorzec(P),
  [opn(')'), opb2('=')], wyrazenie(E).

wzorzec(X) --> wzor(E), wzorzeca(E, X).

wzorzeca(E1, X) -->
  [opb1(',')], !, wzor(E2), wzorzeca(pair(no, E1, E2), X).
wzorzeca(E, E) --> [].

wzor(wildcard(no)) --> [sk('_')], !.
wzor(var(no, X)) --> [id(X)], !.
wzor(X) --> ['('], wzorzec(X), [')'].

wyrazenie(if(no, E1, E2, E3)) -->
  [sk(if)], !, wyrazenie(E1), [sk(then)], wyrazenie(E2),
  [sk(else)], wyrazenie(E3).
wyrazenie(let(no, P, E1, E2)) -->
  [sk(let)], !, wzorzec(P), [opb2('=')], wyrazenie(E1),
  [sk(in)], wyrazenie(E2).
wyrazenie(X) --> wyr_op(X).

wyr_op(X) --> wyr1(E), wyr_opa(E,X).
wyr_opa(E, pair(no, E, X)) --> [opb1(',')], !, wyr_op(X).
wyr_opa(A, A) --> [].

wyr1(X) --> wyr2(E), wyr1a(E,X).
wyr1a(E, op(no, Op, E, X)) --> [opb2(Op)], wyr1(X).
wyr1a(A, A) --> [].

wyr2(X) --> wyr3(E), wyr2a(E,X).
wyr2a(E, op(no, Op, E, X)) --> [opb3(Op)], wyr2(X).
wyr2a(A, A) --> [].

wyr3(X) --> wyr4(Y), wyr3a(Y, X).
wyr3a(X, Y) --> [opb4(Op)], wyr4(Z), wyr3a(op(no, Op, X, Z), Y).
wyr3a(X, X) --> [].

wyr4(X) --> wyr5(Y), wyr4a(Y, X).
wyr4a(X, Y) --> [opb5(Op)], wyr5(Z), wyr4a(op(no, Op, X, Z), Y).
wyr4a(X, X) --> [].

wyr5(op(no, Op, E)) --> [opu(Op)], !, wyru(E).
wyr5(X) --> wyru(X).

wyru(E) --> [opn('(')], !, wyrazenie(E), [opn(')')].
wyru(B) --> atomowe(A), wyru_ab(A, B).

wyru_ab(A, B) --> wyrua(A, B).
wyru_ab(A, B) --> wyrub(A, B).
wyru_ab(A, A) --> [].

wyrua(A, B) -->
  [opn('[')], wyrazenie(C), [opn(']')], !,
  wyru_ab(bitsel(no, A, C), B).

wyrub(A, B) -->
  [opn('[')], wyrazenie(C), [opkk], wyrazenie(D),
  [opn(']')], !, wyru_ab(bitsel(no, A, C, D), B).

atomowe(call(no, Name, E)) -->
  [id(Name), opn('(')], !, wyrazenie(E), [opn(')')].
atomowe(var(no, X)) --> [id(X)], !.
atomowe(num(no, N)) --> [li(N)], !.
atomowe(empty(no)) --> [opn('['), opn(']')], !.
atomowe(bit(no, E)) --> [opn('[')], wyrazenie(E), [opn(']')].

