
sumsq_even([], 0).
sumsq_even([H | T], Sum) :- 
    mod(H, 2) =:= 0,
    sumsq_even(T, Rest),
    Sum is H + Rest.
sumsq_even([H | T], Sum) :-
    mod(H, 2) \== 0,
    sumsq_even(T, Sum).


% sumsq_even([1,3,5,2,-4,6,8,-7], Sum).