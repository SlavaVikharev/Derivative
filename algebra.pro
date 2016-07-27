:- include('draw.pro').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ввод строки с консоли и преобразование ее в список токенов.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_expr(Tokens) :- read_loop([], Tokens).
read_loop(Acc, Tokens) :-
	peek_code(C), C == 10 -> reverse(Acc, Tokens), ! ;
	read_token(Tok), read_loop([Tok | Acc], Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Преобразование списка токенов в дерево.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

factor(un(Op, X)) --> unary_op(Op), factor(X).
factor(num(X)) --> [X], {number(X)}.
factor(id(X)) --> [X], {atom(X)}.
factor(E) --> [punct('(')], expr(E), [punct(')')].
power(bin(Op, X, Y)) --> factor(X), power_op(Op), expr(Y).
power(E) --> factor(E).
term(bin(Op, X, Y)) --> power(X), mul_op(Op), expr(Y).
term(E) --> power(E).
expr(bin(Op, X, Y)) --> term(X), add_op(Op), expr(Y).
expr(E) --> term(E).

unary_op('-') --> ['-'].
power_op('^') --> ['^'].
mul_op('*') --> ['*'].
mul_op('/') --> ['/'].
add_op('+') --> ['+'].
add_op('-') --> ['-'].

parse(Tree) :- read_expr(List), expr(Tree, List, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Вычисление производной.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diff(id(_), num(1)) :- !.
diff(num(_), num(0)) :- !.

diff(un(Op, X), R) :- diff(X, X1), R = un(Op, X1), !.
diff(bin(Op, X, Y), R) :- bin_step(Op, X, Y, R), !.

bin_step(+, X, Y, R) :-
  format('args +:~nX: ~w~nY: ~w~n~n', [X, Y]),
  diff(X, X1), diff(Y, Y1),
  format('derv +:~nX1: ~w~nY1: ~w~n~n', [X1, Y1]),
  R = bin(+, X1, Y1), !.

bin_step(-, X, Y, R) :-
  format('args -:~nX: ~w~nY: ~w~n~n', [X, Y]),
  diff(X, X1), diff(Y, Y1),
  format('derv -:~nX1: ~w~nY1: ~w~n~n', [X1, Y1]),
  R = bin(-, X1, Y1), !.

bin_step(*, X, Y, bin(+, X2, Y2)) :-
  format('args *:~nX: ~w~nY: ~w~n~n', [X, Y]),
  diff(X, X1), diff(Y, Y1),
  format('derv *:~nX1: ~w~nY1: ~w~n~n', [X1, Y1]),
  X2 = bin(*, X, Y1),
  Y2 = bin(*, X1, Y), !.

bin_step(/, X, Y, bin(/, X3, Y3)) :-
  format('args /:~nX: ~w~nY: ~w~n~n', [X, Y]),
  diff(X, X1), diff(Y, Y1),
  format('derv /:~nX1: ~w~nY1: ~w~n~n', [X1, Y1]),
  X2 = bin(*, X, Y1),
  Y2 = bin(*, X1, Y),
  X3 = bin(-, X2, Y2),
  Y3 = bin(^, Y, num(2)), !.

bin_step(^, X, num(Y), bin(*, num(Y), X1)) :-
  Y1 is Y-1, X1 = bin(^, X, Y1), !.

bin_step(^, X, Y, bin(+, X2, Y2)) :-
  format('args ^:~nX: ~w~nY: ~w~n~n', [X, Y]),
  diff(X, X1), diff(Y, Y1),
  format('derv ^:~nX1: ~w~nY1: ~w~n~n', [X1, Y1]),
  T1 = bin(-, Y, num(1)),
  T2 = bin(^, X, T1),
  X2 = bin(*, X1, bin(*, Y, T2)),
  T3 = un(ln, X),
  T4 = bin(^, X, Y),
  Y2 = bin(*, Y1, bin(*, T3, T4)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Преобразование дерева в строку результата.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

priority(+, 1).
priority(-, 1).
priority(*, 2).
priority(/, 2).
priority(^, 3).
priority(ln, 4).

show_num(X) :-
	Y is float(X), Z is float_integer_part(Y),
	Z =:= Y -> (T is truncate(Y), write(T)) ; write(X).

show_op1(num(X)) :- show_num(X).
show_op1(id(X)) :- write(X).
show_op1(X) :- show(X).

show_op2(num(X), _) :- show_num(X).
show_op2(id(X), _) :- write(X).
show_op2(bin(Op, X, Y), N) :- priority(Op, M), M > N -> show(bin(Op, X, Y)).
show_op2(X, _) :- write('('), show(X), write(')').

show(num(X)) :- show_num(X).
show(id(X)) :- write(X).
show(un(Op, X)) :- write(Op), write('('), show_op1(X), write(')').
show(bin(Op, X, Y)) :- priority(Op, N), show_op2(X, N), write(Op), show_op2(Y, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Запуск программы.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(start).
start :- (parse(Tree) ->
  (draw(Tree), diff(Tree, Result), draw(Result), show(Result))
  ; write('Syntax error!')), !.
