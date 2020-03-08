% https://webcms3.cse.unsw.edu.au/COMP3411/20T1/resources/42739

insert(Num, [], NewList) :- NewList is [Num].
insert(Num, [A | [B | C]], NewList) :-
    A < Num, Num < B,
    NewList is [A, Num, B | C].