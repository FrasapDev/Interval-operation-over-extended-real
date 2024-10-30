% Francesco Saponara    886465

% -*- Mode:Prolog -*-
% intar.pl

%---------%---------%---------%---------%---------%---------%---------%


% Interval Arithmetic Library %


%---------%---------%---------%---------%---------%---------%---------%

% Extended Reals Representation
% infinity = infinity NOT implemented to be used everywhere
% pos_infinity = +infinity
% neg_infinity = -infinity
% [] = empty interval

% case test basic operation
test(1) :-
  writeln("default operation with interval I1=[1,2] I2=[2,pos_infinity]"),
  writeln("I1+I2"),
  interval(1, 2, I1),
  interval(2, pos_infinity, I2),
  iplus(I1,I2,R1),
  iminus(I1,I2,R2),
  itimes(I1,I2,R3),
  idiv(I1, I2, R4),
  writeln(R1),
  writeln("I1-I2"),
  writeln(R2),
  writeln("I1*I2"),
  writeln(R3),
  writeln("I1/I2"),
  writeln(R4),
  write("default operation with 1 disjoint interval "),
  writeln("I3=[[1,2][5,7]] I4=[2,11]"),
  interval(5, 7, I0),
  interval(2, 11, I4),
  I3 = [I1,I0],
  iplus(I3,I4,R5),
  iminus(I3,I4,R6),
  itimes(I3,I4,R7),
  idiv(I3, I4, R8),
  writeln("I3+I4"),
  writeln(R5),
  writeln("I3-I4"),
  writeln(R6),
  writeln("I3*I4"),
  writeln(R7),
  writeln("I3/I4"),
  writeln(R8),
  writeln("test contains I2=[2,pos_infinity] I4=[2,11]"),
  writeln(icontains(I2, I4)),
  writeln("test contains I4=[2,11] I2=[2,pos_infinity] "),
  icontains(I2, I4),
  writeln("test I5=[pos_infinity,pos_infinity]"),
  interval(pos_infinity, pos_infinity, I5),
  writeln(I5),
  writeln("test I6=[neg_infinity,neg_infinity]"),
  interval(neg_infinity, neg_infinity, I6),
  writeln(I6),
  writeln("test if I7=[pos_infinity,8] then I7=[]"),
  interval(pos_infinity, 8, I7),
  writeln(I7),
  write("default operation with 2 disjoint interval "),
  writeln("I3=[[1,2],[5,7]] I8=[[-7,-1],[2,pos_infinity]]"),
  I8 = [[-7,-1],[2,pos_infinity]],
  iplus(I3,I8,R9),
  iminus(I3,I8,R10),
  itimes(I3,I8,R11),
  idiv(I3, I8, R12),
  writeln("I3+I8"),
  writeln(R9),
  writeln("I3-I8"),
  writeln(R10),
  writeln("I3*I8"),
  writeln(R11),
  writeln("I3/I8"),
  writeln(R12),
  write("test isup e iinf I9=[[[neg_infinity,-9],[1,2]]"),
  writeln(",[[3,4],[5,pos_infinity]]]"),
  I9=[[[neg_infinity,-9],[1,2]],[[3,4],[5,pos_infinity]]],
  iinf(I9, INF),
  isup(I9, SUP),
  writeln(INF),
  writeln(SUP).


% Predicate plus_e/1: Unit of summation operation
plus_e(0). % The unit of addition is 0

% Predicate plus_e/2: True when Result unifies with X
plus_e(X, R) :-  ground(X), extended_real(X),
                 X=R, extended_real(R).
                % X must be an extended real

% Arithmetic operations for extended reals
% Predicate plus_e/3

plus_e(X, Y, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == pos_infinity,
                    X \= neg_infinity, R = Y, extended_real(R), !.
plus_e(Y, X, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == pos_infinity,
                    X \= neg_infinity, R = Y, !.
plus_e(Y, X, R) :- Y == neg_infinity, ground(X),
                    ground(Y), extended_real(Y), !,
                    extended_real(X), X \= pos_infinity, R = Y, !.
plus_e(X, Y, R) :- Y == neg_infinity, ground(X),
                    ground(Y), extended_real(Y), !,
                    extended_real(X), X \= pos_infinity, R = Y, !.
plus_e(X, Y, R) :- X \= pos_infinity, X \= neg_infinity,
                    Y \= pos_infinity,
                    Y \= neg_infinity, ground(X),
                    ground(Y), extended_real(X),
                    extended_real(Y), R is X + Y, !.
plus_e(_, _, []) :- fail.

% Predicate minus_e/2

minus_e(X, Y) :- ground(X), extended_real(X),
                    extended_real(Y), X == pos_infinity,
                    Y = neg_infinity.
minus_e(X, Y) :- ground(X), extended_real(X),
                    extended_real(Y), X == infinity,
                    Y = neg_infinity.
minus_e(X, Y) :- ground(X), extended_real(X),
                    extended_real(Y), X == neg_infinity,
                    Y = pos_infinity.
minus_e(X, Y) :- ground(X), extended_real(X), X \= pos_infinity,
                    X \= neg_infinity, Y is 0 - X.


% Predicate minus_e/3
minus_e(X, Y, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == pos_infinity,
                    X \= pos_infinity, minus_e(Y,R), !.
minus_e(Y, X, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == pos_infinity,
                    X \= pos_infinity, R = Y, !.
minus_e(Y, X, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == neg_infinity,
                    X \= neg_infinity, R = Y, !.
minus_e(X, Y, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), Y == neg_infinity,
                    X \= neg_infinity, minus_e(Y,R), !.
minus_e(X, Y, R) :- X \= pos_infinity, X \= neg_infinity,
                    Y \= pos_infinity, Y \= neg_infinity,
                    ground(X), ground(Y), extended_real(X),
                    extended_real(Y), R is X - Y, !.
minus_e(_, _, []) :- fail.

% Predicate times_e/1: Unit of summation operation
times_e(1).

% Predicate times_e/2: True when Result unifies with X
times_e(X, R) :-  ground(X), extended_real(X),
                    extended_real(R), R == X.
                    % X must be an extended real

% Predicate times_e/3
times_e(X, Y, R) :- ground(X), ground(Y),
                    extended_real(X), extended_real(Y),
                    times__e(X, Y, R).
times__e(Y, X, R) :- Y == 0, X \= pos_infinity,
                        X \= neg_infinity, R = 0, !.
times__e(X, Y, R) :- Y == 0, X \= pos_infinity,
                        X \= neg_infinity, R = 0, !.
times__e(Y, X, R) :- Y == pos_infinity,
                        X == pos_infinity, R = pos_infinity, !.
times__e(Y, X, R) :- X == neg_infinity,
                        Y == pos_infinity, R = neg_infinity, !.
times__e(Y, X, R) :- X == pos_infinity,
                        Y == neg_infinity, R = neg_infinity, !.
times__e(Y, X, R) :- Y == neg_infinity,
                        X == neg_infinity, R = pos_infinity, !.
times__e(Y, X, R) :- Y == pos_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = pos_infinity, X > 0, !.
times__e(X, Y, R) :- Y == pos_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = pos_infinity, X > 0, !.
times__e(Y, X, R) :- Y == neg_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = neg_infinity, X > 0, !.
times__e(X, Y, R) :- Y == neg_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = neg_infinity, X > 0, !.
times__e(Y, X, R) :- Y == pos_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = neg_infinity, X < 0, !.
times__e(X, Y, R) :- Y == pos_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = neg_infinity, X < 0, !.
times__e(Y, X, R) :- Y == neg_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = pos_infinity, X < 0, !.
times__e(X, Y, R) :- Y == neg_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        R = pos_infinity, X < 0, !.
times__e(X, Y, R) :- X \= pos_infinity,
                        X \= neg_infinity, Y \= pos_infinity,
                        Y \= neg_infinity, R is X * Y, !.
times__e(_, _, []) :- fail.

% Predicate div_e/2
div_e(X,R) :- X == pos_infinity, ground(X),
                extended_real(X), R is 0, extended_real(R);
                X == neg_infinity, ground(X), extended_real(X),
                R is 0, extended_real(R);
                X == infinity, ground(X), extended_real(X),
                R is 0, extended_real(R).
div_e(X,R) :- X \= 0, X \= pos_infinity, X \= infinity,
                X \= neg_infinity, ground(X),
                extended_real(X), R is 1 / X.
div_e(X,_) :- X == 0, fail.

% Predicate div_e/3
div_e(X, Y, R) :- ground(X), ground(Y), extended_real(X),
                    extended_real(Y), div__e(X, Y, R).
div__e(Y, X, R) :- Y == 0, X \= 0, R is 0, !.
div__e(X, Y, R) :- X \= pos_infinity, X \= neg_infinity,
                    Y == pos_infinity, R = 0, X \= 0, !.
div__e(X, Y, R) :- X \= pos_infinity, X \= neg_infinity,
                    Y == neg_infinity, R = 0, X \= 0, !.
div__e(Y, X, R) :- Y == pos_infinity, X \= pos_infinity,
                    X \= neg_infinity, R = pos_infinity, X > 0, !.
div__e(Y, X, R) :- Y == neg_infinity, X \= pos_infinity,
                    X \= neg_infinity, R = neg_infinity, X > 0, !.
div__e(Y, X, R) :- Y == neg_infinity, X \= pos_infinity,
                    X \= neg_infinity, R = pos_infinity, X < 0, !.
div__e(Y, X, R) :- Y == pos_infinity, X \= pos_infinity,
                    X \= neg_infinity, R = neg_infinity, X < 0, !.
div__e(X, Y, R) :- X \= pos_infinity, X \= neg_infinity,
                    Y \= pos_infinity, Y \= neg_infinity, Y \= 0,
                    R is X / Y, !.
div__e(_, _, []) :- fail.


% Interval Construction and Other Predicates


% empty_interval/1
empty_interval([]).

% interval/1
interval([]).

% interval/2
interval(X, SI) :- SI = [X,X], ground(X), extended_real(X),
                    X \= pos_infinity,
                    X \= neg_infinity.

% interval/3
interval(L, H, I) :- extended_real(L), extended_real(H),
                        ground(L), ground(H), i_nterval(L,H,I).
i_nterval(L, H, I) :- H == pos_infinity, I = [L, H].
i_nterval(L, H, I) :- L == neg_infinity, I = [L, H].
i_nterval(L, H, I) :- L == pos_infinity, H \= pos_infinity, I = [].
i_nterval(L, H, I) :- H == neg_infinity, L \= neg_infinity, I = [].
i_nterval(L, H, I) :- L == infinity, H == infinity, I = [L,H].
i_nterval(L, H, I) :- L \= neg_infinity, H \= neg_infinity,
                        L \= pos_infinity, H \= pos_infinity,
                        L =< H, I = [L, H].
i_nterval(L, H, I) :- L \= neg_infinity, H \= neg_infinity,
                        L \= pos_infinity, H \= pos_infinity,
                        L > H, I = [].

% is_interval/1
is_interval(I) :- ground(I), is__interval(I).
is_interval(I) :- ground(I), I == [].
is__interval(I) :- ground(I), is_dis_interval(I).
is__interval([X, X]) :- extended_real(X), X \= pos_infinity,
                        X \= neg_infinity.
is__interval([L, H]) :- L == neg_infinity,
                        extended_real(L),
                        extended_real(H), !.
is__interval([L, H]) :- H == pos_infinity,
                        extended_real(L),
                        extended_real(H), !.
is__interval([L, H]) :- L \= pos_infinity, L \= neg_infinity,
                        H \= pos_infinity,
                        H \= neg_infinity, extended_real(L),
                        extended_real(H), L =< H.

% intervalli disgiunti
is_dis_interval([[L1,H1], [L2, H2]]) :- is__interval([L1,H1]),
                                      is__interval([H1,L2]),
                                      is__interval([L2,H2]).

is_dis_interval([[L1,H1], [L2, H2]]) :- is__interval([L1,H1]),
                                        is_dis_interval([L2,H2]).

is_dis_interval([[L1,H1], [L2, H2]]) :- is_dis_interval([L1,H1]),
                                        is__interval([L2,H2]).

% whole_interval/1
whole_interval([neg_infinity, pos_infinity]).

% is_singleton/1
is_singleton(I):- ground(I), is__singleton(I).
is__singleton([X, X]) :- extended_real(X).

% iinf/2
iinf([L, H], X) :- is_interval([L, H]), is_interval(L), iinf(L,X).
iinf([L, H], L) :- is_interval([L, H]).

% isup/2
isup([L, H], X) :- is_interval([L, H]), is_interval(H), isup(H,X).
isup([L, H], H) :- is_interval([L, H]).

% icontains/2
icontains(X,[]) :-  is_interval(X), !.

icontains([L, H], _) :- is_interval([L, H]),
                        L == neg_infinity,
                        H == pos_infinity.

icontains([L, H], X) :- extended_real(X), is_interval([L, H]),
                        X == pos_infinity, H == pos_infinity.
icontains([L, H], X) :- extended_real(X), is_interval([L, H]),
                        X == neg_infinity, L == neg_infinity.
icontains([L, H], X) :- extended_real(X), is__interval([L, H]),
                        L == neg_infinity, X =< H.
icontains([L, H], X) :- extended_real(X), is__interval([L, H]),
                        H == pos_infinity, X >= L.
icontains([L, H], X) :- extended_real(X), is__interval([L, H]),
                        L \= pos_infinity, L \= neg_infinity,
                        H \= pos_infinity, H \= neg_infinity,
                        X \= pos_infinity, X \= neg_infinity,
                        X >= L, X =< H, !.


icontains([L, H], [X, Xs]) :- is__interval([L, H]), is__interval([X, Xs]),
                                X == pos_infinity, L == pos_infinity.
icontains([L, H], [X, Xs]) :- is__interval([L, H]), is__interval([X, Xs]),
                                Xs == neg_infinity, H == neg_infinity.
icontains([L, H], [X, Xs]) :- is__interval([L, H]), is__interval([X, Xs]),
                                L == neg_infinity,
                                H \= pos_infinity, H \= neg_infinity,
                                Xs \= pos_infinity, Xs \= neg_infinity,
                                Xs =< H.

icontains([L, H], [X, Xs]) :- is__interval([L, H]), is__interval([X, Xs]),
                            H == pos_infinity,
                            L \= pos_infinity, L \= neg_infinity,
                            X \= pos_infinity, X \= neg_infinity,
                            X >= L.

icontains([L, H], [X, Xs]) :- L \= pos_infinity, L \= neg_infinity,
                            H \= pos_infinity, H \= neg_infinity,
                            X \= pos_infinity, X \= neg_infinity,
                            Xs \= pos_infinity, Xs \= neg_infinity,
                            is__interval([L, H]), is__interval([X, Xs]),
                            X >= L, Xs =< H.

icontains([L, H], [X, Xs]) :- is_dis_interval([X,Xs]),
                              icontains([L,H], X), icontains([L,H], Xs).


icontains([L, H], [X, Xs]) :- is_dis_interval([L,H]),
                              (icontains(L, [X,Xs]); icontains(H, [X,Xs])).



icontains([],_) :- fail, !.

% ioverlap/2
ioverlap(X, Y) :- icontains(X,Y), !.
ioverlap(X, Y) :- icontains(Y,X), !.

ioverlap(X, Y) :- is_interval(X), is_interval(Y),
                  iinf(X,L1), iinf(Y, L2),
                  isup(X, H1), isup(Y, H2),
                  (icontains(X, L2); icontains(X, H2);
                  icontains(Y, L1); icontains(Y, H1)).



ioverlap(_,_) :- fail, !.


% Interval Arithmetic Predicates

% iplus/1
iplus(I) :- is_interval(I).

% iplus/2
iplus([],_) :- fail.
iplus(X, R) :- ground(X), is_interval(X), R = X.
iplus(X, R) :- ground(X), extended_real(X), R = [X,X].

% iplus/3
iplus(X, Y, R) :-
    extended_real(X),
    interval(X,X2),
    iplus(X2,Y,R).

iplus(X, Y, R) :-
    extended_real(Y),
    interval(Y,Y2),
    iplus(X, Y2, R).

iplus(X, Y, R) :- ground(X), ground(Y),
                    is__interval(X),is__interval(Y), i_plus(X,Y,R).

i_plus([L1, H1], [L2, H2], R) :- is_dis_interval([L1, H1]),
     is_dis_interval([L2, H2]),
        iplus(L1, L2, X), iplus(H1, H2, Y), R = [X, Y].

i_plus([L1, H1], [L2, H2], R) :- is_dis_interval([L1, H1]),
  iplus(L1, L2, X), iplus(H1, H2, Y), R = [X, Y].

i_plus([L1, H1], [L2, H2], R) :- is_dis_interval([L2, H2]),
  iplus(L1, L2, X), iplus(H1, H2, Y), R = [X, Y].

i_plus([L1, H1], [L2, H2], [L3, H3]) :-
    plus_e(L1, L2, L3),
    plus_e(H1, H2, H3).


% iminus/2
iminus(X, [Y, Y]) :- ground(X), extended_real(X), minus_e(X, Y).
iminus(X, Y) :- ground(X), i_minus(X,Y).
i_minus([L1,H1],R) :- is_dis_interval([L1,H1]),
                            iminus(L1, X), iminus(H1, Y), R = [Y,X].
i_minus([L1,H1],R) :- is__interval([L1,H1]),
                            minus_e(L1,X), minus_e(H1,Y), R = [Y,X].

% iminus/3

iminus(X, Y, R) :-
    ground(X),
    extended_real(X),
    interval(X,X2),
    iminus(X2,Y,R).

iminus(X, Y, R) :-
    ground(Y),
    extended_real(Y),
    interval(Y,Y2),
    iminus(X, Y2, R).

iminus(X, Y, R) :- ground(X), ground(Y),
                    is__interval(X),is__interval(Y), i_minus(X,Y,R).

i_minus([L1, H1], [L2, H2], R) :- is_dis_interval([L1, H1]),
    is_dis_interval([L2, H2]),
    iminus(L1, H2, X), iminus(H1, L2, Y), R = [X, Y].

i_minus([L1, H1], [L2, H2], R) :- is_dis_interval([L1, H1]),
    iminus(L1, H2, X), iminus(H1, L2, Y), R = [X, Y].

i_minus([L1, H1], [L2, H2], R) :- is_dis_interval([L2, H2]),
    iminus(L1, H2, X), iminus(H1, L2, Y), R = [X, Y].

i_minus([L1, H1], [L2, H2], [L3, H3]) :-
    minus_e(L1, H2, L3),
    minus_e(H1, L2, H3).




% itimes/1
itimes(ZI) :- is__interval(ZI).

% itimes/2
itimes([],_) :- fail.
itimes(X, R) :- ground(X), is__interval(X), R = X.
itimes(X, R) :- ground(X), extended_real(X), R = [X,X].

% itimes/3
itimes(X, Y, R) :-
    extended_real(X),
    interval(X,X2),
    itimes(X2,Y,R).

itimes(X, Y, R) :-
    extended_real(Y),
    interval(Y,Y2),
    itimes(X, Y2, R).

itimes(X,Y,R) :- ground(X), ground(Y),
                is__interval(X),is__interval(Y), i_times(X,Y,R).

list_min([L|H], Min) :-
    is_interval(L),
    list_min(L,M1),
    list_min(H,M2),
    list_min([M1,M2], Min).

list_min([L|Ls], Min) :-
    list_min(Ls, L, Min).


list_min([], Min, Min).
list_min([L|H], P, Min) :-
        is_interval(L),
        list_min(L,P,M1),
        list_min(H,P,M2),
        list_min([M1,M2], Min).
list_min([L|Ls], Min0, Min) :-
    Min0 == neg_infinity, Min = neg_infinity;
    L == neg_infinity, Min = neg_infinity;
    L == pos_infinity, number(Min0), list_min(Ls, Min0, Min);
    L == pos_infinity, list_min(Ls, L, Min);
    Min0 == pos_infinity, list_min(Ls, L, Min).

list_min([L|Ls], Min0, Min):-
    number(L),
    number(Min0),
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).


list_max([L|H], Max) :-
    is_interval(L),
    list_max(L,M1),
    list_max(H,M2),
    list_max([M1,M2], Max).

list_max([L|Ls], Max) :-
    list_max(Ls, L, Max).


list_max([], Max, Max).
list_max([L|H], P, Max) :-
    is_interval(L),
    list_max(L,P,M1),
    list_max(H,P,M2),
    list_max([M1,M2], Max).
list_max([L|Ls], Max0, Max) :-
    Max0 == pos_infinity, Max = pos_infinity;
    L == pos_infinity, Max = pos_infinity;
    L == neg_infinity, number(Max0), list_max(Ls, Max0, Max);
    L == neg_infinity, list_max(Ls, L, Max);
    Max0 == neg_infinity, list_max(Ls, L, Max).

list_max([L|Ls], Max0, Max):-
    number(L),
    number(Max0),
    Max1 is max(L, Max0),
    list_max(Ls, Max1, Max).





i_times([L1, H1], [L2, H2], [L3,H3]) :-
            is_dis_interval([L1,H1]),
            itimes(L1,L2,S1),
            itimes(L1,H2,S2),
            itimes(H1,L2,S3),
            itimes(H1,H2,S4),
            S=[S1,S2,S3,S4],
            list_min(S, L3),
            list_max(S, H3).

i_times([L1, H1], [L2, H2], [L3,H3]) :-
            is_dis_interval([L2,H2]),
            itimes(L1,L2,S1),
            itimes(L1,H2,S2),
            itimes(H1,L2,S3),
            itimes(H1,H2,S4),
            S=[S1,S2,S3,S4],
            list_min(S, L3),
            list_max(S, H3).


i_times([L1, H1], [L2, H2], [L3, H3]):-
            times_e(L1,L2,S1),
            times_e(L1,H2,S2),
            times_e(H1,L2,S3),
            times_e(H1,H2,S4),
            S=[S1,S2,S3,S4],
            list_min(S, L3),
            list_max(S, H3).


% idiv/2
idiv(X, R) :- ground(X), is__interval(X), idiv([1,1],X,R).
idiv(X, R) :- ground(X), extended_real(X), interval(X,X2), idiv([1,1],X2,R).

% idiv/3
idiv(X, Y, R) :-
    extended_real(X),
    interval(X,X2),
    idiv(X2,Y,R).

idiv(X, Y, R) :-
    extended_real(Y),
    interval(Y,Y2),
    idiv(X, Y2, R).

idiv(X,Y,R) :- ground(X), ground(Y),
                is_interval(X),is_interval(Y), i_div(X,Y,R).

i_div([L1, H1], [L2, H2], [L3,H3]) :-
            is_dis_interval([L1,H1]),
            idiv(L1,L2,S1),
            idiv(L1,H2,S2),
            idiv(H1,L2,S3),
            idiv(H1,H2,S4),
            S=[S1,S2,S3,S4],
            list_min(S, L3),
            list_max(S, H3).

i_div([L1, H1], [L2, H2], [L3,H3]) :-
            is_dis_interval([L2,H2]),
            idiv(L1,L2,S1),
            idiv(L1,H2,S2),
            idiv(H1,L2,S3),
            idiv(H1,H2,S4),
            S=[S1,S2,S3,S4],
            list_min(S, L3),
            list_max(S, H3).

% Z, Z
i_div([L1,H1],[L2,H2],_) :- L1 == 0, L2 == 0, H1 == 0, H2 == 0, fail, !.

i_div([L1,H1],[L2,H2],[L3,H3]) :- L1 == 0, H1 == 0, L2 \= 0, H2 \= 0,
                                number(L2), number(H2), L3 = 0, H3 = 0.

% P1, P
i_div([L1,H1],[L2,H2],[L3,H3]) :-   is_pos(L2), is_pos(L1),
                                    div_e(L1,H2,L3),
                                    div_e(H1,L2,H3);
                                    L2 == 0, is_pos(L1),
                                    div_e(L1,H2,L3),
                                    H3 = pos_infinity.

% P0, P
i_div([L1,H1],[L2,H2],[L3,H3]) :- L1 == 0, is_pos(H1),
                                  is_pos(L2), is_pos(H2),
                                  L3 = 0, div_e(H1,L2,H3);
                                  L1 == 0, is_pos(H1), L2 == 0,
                                  is_pos(H2), L3 = 0, H3 = pos_infinity.

% M, P
i_div([L1,H1],[L2,_],[L3,H3]) :- is_neg(L1), is_pos(H1),
                                 is_pos(L2), div_e(L1,L2,L3),
                                 div_e(H1,L2,H3);
                                 is_neg(L1), is_pos(H1),
                                 L2 == 0, L3 = neg_infinity,
                                 H3 = pos_infinity.

% N0, P
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), H1 == 0, is_pos(L2),
                                  div_e(L1,L2,L3), H3 = 0;
                                  is_neg(L1), H1 == 0, L2 == 0,
                                  is_pos(H2), L3 = neg_infinity, H3 = 0.

% N1, P
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(H1), is_pos(L2),
                                  div_e(L1,L2,L3), div_e(H1,H2,H3);
                                  is_neg(H1), L2 == 0, is_pos(H2),
                                  L3 = neg_infinity, div_e(H1,H2,H3).

% P1, M
i_div([L1,_],[L2,H2],[L3,H3]) :- is_pos(L1), is_neg(L2), is_pos(H2),
                                 div_e(L1,L2,X), L3 = [neg_infinity, X],
                                 div_e(L1,H2,Y), H3 = [Y, pos_infinity].

% P0, M
i_div([L1,H1],[L2,H2],[L3,H3]) :- L1 == 0, is_pos(H1), is_neg(L2),
                                  is_pos(H2), L3 = neg_infinity,
                                  H3 = pos_infinity.

% M, M
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), is_pos(H1), is_neg(L2),
                                  is_pos(H2), L3 = neg_infinity,
                                  H3 = pos_infinity.

% N0, M
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), H1 == 0, is_neg(L2),
                                  is_pos(H2), L3 = neg_infinity,
                                  H3 = pos_infinity.

% N1, M
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), is_neg(H1), is_neg(L2),
                                  is_pos(H2), div_e(H1,H2,X),
                                  L3 = [neg_infinity, X],
                                  div_e(H1,L2,Y),
                                  H3 = [Y, pos_infinity].

% P1, N
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_pos(L1), is_neg(H2),
                                  div_e(H1,H2,L3), div_e(L1,L2,H3);
                                  is_pos(L1), H2 == 0, is_neg(L2),
                                  L3 = neg_infinity, div_e(L1,L2,H3).

% P0, N
i_div([L1,H1],[L2,H2],[L3,H3]) :- L1 == 0, is_pos(H1), is_neg(H2),
                                  div_e(H1,H2,L3), H3 = 0;
                                  L1 == 0, is_pos(H1), H2 == 0,
                                  is_neg(L2), L3 = neg_infinity,
                                  H3 = 0.

% M, N
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), is_pos(H1),
                                  is_neg(H2), div_e(H1,H2,L3),
                                  div_e(L1,H2,H3);
                                  is_neg(L1), is_pos(H1),
                                  H2 == 0, is_neg(L2),
                                  L3 = neg_infinity,
                                  H3 = pos_infinity.

% N0, N
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(L1), H1 == 0, is_neg(H2),
                                  L3 = 0, div_e(L1,H2,H3);
                                  is_neg(L1), H1 == 0,
                                  H2 == 0, is_neg(L2),
                                  L3 = 0, H3 = pos_infinity.

% N1, N
i_div([L1,H1],[L2,H2],[L3,H3]) :- is_neg(H1), is_neg(H2),
                                  div_e(H1,L2,L3), div_e(L1,H2,H3);
                                  is_neg(H1), H2 == 0, is_neg(L2),
                                  div_e(H1,L2,L3), H3 = pos_infinity.



% is_pos return true if X is a pos_extended_real
is_pos(X) :- X == pos_infinity.
is_pos(X) :- X == infinity.
is_pos(X) :- number(X), X > 0.

% is_neg return true if X is a neg_extended_real
is_neg(X) :- X == neg_infinity.
is_neg(X) :- number(X), X < 0.

% Utility Predicate
extended_real(X) :-
    number(X); member(X, [infinity, pos_infinity, neg_infinity]).
