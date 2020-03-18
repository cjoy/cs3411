% Assignment 1 - Part 1 Prolog 
% COMP3411 20T1, 20th of March
% Chris Joy, z5113243


% Q1.1: sumsq_even(Numbers, Sum) - sums the squares of only the even numbers in a list of integers
sumsq_even([], 0).
sumsq_even([H | T], Sum) :- 
    even(H),
    sumsq_even(T, Rest),
    Sum is (H * H) + Rest.
sumsq_even([H | T], Sum) :-
    odd(H),
    sumsq_even(T, Sum).


% Q1.2: log_table(NumberList, ResultList) - lists pairs of a number and its log
log_table([], []).
log_table([H | T], ResultList) :-
    log_table(T, Rest),
    X is log(H),
    append([[H, X]], Rest, ResultList).


% Q1.3: paruns(List,RunList) - converts a list of numbers into the corresponding list of parity runs
paruns([],[]).
paruns([A],[[A]]).
paruns([A,B|Rest], [[A|A1]|Rest2]) :-
    even(A),
    even(B),
    paruns([B|Rest], [A1|Rest2]).
paruns([A,B|Rest], [[A|A1]|Rest2]) :-
    odd(A),
    odd(B),
    paruns([B|Rest], [A1|Rest2]).
paruns([A,B|Rest], [[A]|Rest2]) :-
    even(A),
    odd(B),
    paruns([B|Rest], Rest2).
paruns([A,B|Rest], [[A]|Rest2]) :-
    odd(A),
    even(B),
    paruns([B|Rest], Rest2).


% Q1.4: eval(Expr, Val) - evaluates an expression such as eval(mul(sum(3, 5), 1) Num) --> Num = 15
eval(A, Val) :-
    number(A),
    Val is A.
eval(add(A, B), Val) :-
    eval(A, X),
    eval(B, Y),
    Val is X + Y.
eval(sub(A, B), Val) :-
    eval(A, X),
    eval(B, Y),
    Val is X - Y.
eval(mul(A, B), Val) :-
    eval(A, X),
    eval(B, Y),
    Val is X * Y.
eval(div(A, B), Val) :-
    eval(A, X),
    eval(B, Y),
    Val is X / Y.


% Miscellaneous helpers
even(N) :- 0 is N mod 2.
odd(N) :- not(even(N)).
