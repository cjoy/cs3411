% Assignment 1: Prolog and Search | COMP3411 20T1
% Chris Joy, z5113243

% Q1.1: sumsq_even(Numbers, Sum) - sums the squares of only the even numbers in a list of integers
sumsq_even([], 0).
sumsq_even([H | T], Sum) :- 
    mod(H, 2) =:= 0,
    sumsq_even(T, Rest),
    Sum is H*H + Rest.
sumsq_even([H | T], Sum) :-
    mod(H, 2) \== 0,
    sumsq_even(T, Sum).

% Q1.2: log_table(NumberList, ResultList) - that binds ResultList to the list of pairs consisting of a number and its log, for each number in NumberList
log_table([], []).
log_table([H | T], ResultList) :-
    log_table(T, Rest),
    X is log(H),
    append([[H, X]], Rest, ResultList).

% Q1.3: paruns(List,RunList) - converts a list of numbers into the corresponding list of parity runs
paruns([],[]).
paruns([Head | Tail], [E, O | S]) :-
   even([Head | Tail], E, Last),
   odd(Last, O, Last1),
   paruns(Last1, S).
% paruns([Head | Tail], R) :-
%    even([Head | Tail], E, Last),
%    odd(Last, O, Last1),
%     (E=[] -> R=[O|S] ; O=[] -> R=[E|S] ; R=[E,O|S]),
%    paruns(Last1, S).

even([],[],[]).
even([Head | Tail], [], [Head | Tail]) :-
   Head mod 2 =:= 1.
even([Head | Tail], [Head | L], Last) :-
   Head mod 2 =:= 0,
   even(Tail, L, Last).

odd([],[],[]). 
odd([Head | Tail], [], [Head | Tail]) :-
   Head mod 2 =:= 0.
odd([Head | Tail], [Head | L], Last) :-
   Head mod 2 =:= 1,
   odd(Tail, L, Last).


% Q1.4: eval(Expr, Val) - that will evaluate an expression
eval(A, Val) :- number(A), Val is A.
eval(add(A, B), Val) :- eval(A, X), eval(B, Y), Val is X + Y.
eval(sub(A, B), Val) :- eval(A, X), eval(B, Y), Val is X - Y.
eval(mul(A, B), Val) :- eval(A, X), eval(B, Y), Val is X * Y.
eval(div(A, B), Val) :- eval(A, X), eval(B, Y), Val is X / Y.