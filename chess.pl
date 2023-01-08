%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* inicializar o tabuleiro de jogo */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% peças brancas
initial_position('A', 2, 'WP').
initial_position('B', 2, 'WP').
initial_position('C', 2, 'WP').
initial_position('D', 2, 'WP').
initial_position('E', 2, 'WP').
initial_position('F', 2, 'WP').
initial_position('G', 2, 'WP').
initial_position('H', 2, 'WP').
initial_position('A', 1, 'WR').
initial_position('B', 1, 'WN').
initial_position('C', 1, 'WB').
initial_position('D', 1, 'WQ').
initial_position('E', 1, 'WK').
initial_position('F', 1, 'WB').
initial_position('G', 1, 'WN').
initial_position('H', 1, 'WR').

% peças pretas
initial_position('A', 7, 'BP').
initial_position('B', 7, 'BP').
initial_position('C', 7, 'BP').
initial_position('D', 7, 'BP').
initial_position('E', 7, 'BP').
initial_position('F', 7, 'BP').
initial_position('G', 7, 'BP').
initial_position('H', 7, 'BP').
initial_position('A', 8, 'BR').
initial_position('B', 8, 'BN').
initial_position('C', 8, 'BB').
initial_position('D', 8, 'BQ').
initial_position('E', 8, 'BK').
initial_position('F', 8, 'BB').
initial_position('G', 8, 'BN').
initial_position('H', 8, 'BR').

:- dynamic(actual_position/3).

init :- (initial_position(X, Y,P), \+(actual_position(X, Y, P)),
    asserta(actual_position(X, Y, P)),init);!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* desenhar o tabuleiro do jogo */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(n_jogadas/1).

start :- init, draw_board(2), asserta(n_jogadas(1)), gets, mostrar.

mostrar :- draw_board(2).

draw_board(Linha):-
    (Linha=<18,
    ((Y is Linha mod 2,Y=1, write_line(34),nl);
    (Y is Linha mod 2,Y=0,write_column(Linha,0),nl)),
    LinhaN is Linha+1,draw_board(LinhaN));!.

write_line(X):-(X>0,write('-'),write_line(X-1));!.

write_column(2,X):- write('   A   B   C   D   E   F   G   H').

%alterada
write_column(Y,X) :-
    (X=<33,
    ((check_trace(X,0),write('|'),Z is X+1);
    (check_space_piece(X,0),write(' '),Z is X+1);
    (X=0,W is truncate(Y/2-1),write(W),Z is X+1);
    (check_piece(Y,X), Z is X+2);
    (write(' '),Z is X+1)),
    write_column(Y,Z));!.

check_trace(Pos,Max):-
    Max=<10,((Y is Pos-4*Max,Y=1,!);(MaxN is Max+1,check_trace(Pos,MaxN))).

check_space(Pos,Max):-
    Max=<10,(((Y is Pos-4*Max,Y=2,!);
    (Y is Pos-4*Max,Y=4,!));(MaxN is Max+1,check_space(Pos,MaxN))).

check_letter(Pos,Max):-
    Max=<8,((Y is Pos-4*Max,Y=3,!);(MaxN is Max+1,check_letter(Pos,MaxN))).

check_space_piece(Pos,Max):-
    Max=<8,(((Y is Pos-4*Max,Y=4,!));
    (MaxN is Max+1,check_space_piece(Pos,MaxN))).

check_piece(Y,X):-
    Z is truncate((Y/2) - 1),
    ASCII is truncate(64+(X+2)/4),
    actual_position(Letra, Z, Peca),
    char_code(Letra, ASCII),
    write(Peca).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* regras das peças */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* peão branco */

% testar validade da jogada (sem conseqquência no jogo)
pieces_rules([87, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    X_actual = X_next,
    ((Y is Y_actual + 1, Y_next = Y,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+actual_position(L, Y_next,_));
    (Y_actual = 2, Y_next = 4,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+actual_position(L, Y_next, _),
    Z is Y_next + 1, !, \+actual_position(L, Z,_))),
    Play is 0.

% realizar a jogada
pieces_rules([87, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((X is X_actual + 1, X = X_next); 
    (X is X_actual - 1, X = X_next)),
    Y is Y_actual + 1, Y = Y_next,
    ASCII is 64 + X_next, char_code(L, ASCII),
    actual_position(L, Y_next, P),
    name(P, [66,_]),
    Play is 1.


/* peão preto */

% testar validade da jogada (sem conseqquência no jogo)
pieces_rules([66, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    X_actual = X_next,
    ((Y is Y_actual - 1, Y_next = Y,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+actual_position(L, Y_next,_));
    (Y_actual = 7, Y_next = 5,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+actual_position(L, Y_next,_),
    Z is Y_next - 1, !, \+actual_position(L, Z, _))),
    Play is 0.

% realizar a jogada
pieces_rules([66, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((X is X_actual + 1, X = X_next);
    (X is X_actual - 1, X = X_next)),
    Y is Y_actual + 1, Y = Y_next,
    ASCII is 64 + X_next, char_code(L, ASCII),
    actual_position(L, Y_next, P),
    name(P, [87,_]),
    Play is 1.


/* torre preta ou branca */

% testar e realizar a jogada
pieces_rules([C, 82], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((var(X_next), (!, digito(N), (
    (X_next is X_actual + N, Y_next is Y_actual);
    (((N =< X_actual, X_next is X_actual - N); (N > X_actual, X_next is N - X_actual)), Y_next is Y_actual);
    (((N =< Y_actual, Y_next is Y_actual - N); (N > Y_actual, Y_next is N - Y_actual)), X_next is X_actual))),
    X_next > 0, X_next < 9, 
    Y_next > 0, Y_next < 9);
    (\+var(X_next),
    ((X_actual = X_next); (Y_actual = Y_next)))),
    ASCII is 64 + X_next, char_code(L, ASCII),
    ((\+actual_position(L, Y_next,_),
    check_piece_existence([X_actual, Y_actual], [X_next, Y_next]),
    Play is 0);
    (actual_position(L, Y_next, P),
    \+name(P, [C,_]), check_piece_existence([X_actual, Y_actual], [X_next, Y_next]),
    Play is 1)).


/* Bispo preto ou branco */

% testar e realizar a jogada
pieces_rules([C,66], [X_actual,Y_actual], [X_next,Y_next], Play) :-
    ((var(X_next), (!, digito(N),
    ((X_next is X_actual + N, (Y_next is Y_actual + N; Y_next is Y_actual - N));
    (X_next is X_actual - N, (Y_next is Y_actual + N; Y_next is Y_actual - N)))),
    X_next > 0, X_next < 9, Y_next > 0, Y_next < 9);
    (\+var(X_next),
    (X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next), X_dif = Y_dif))),
    ASCII is 64 + X_next, char_code(L, ASCII),
    ((\+actual_position(L, Y_next,_),
    check_piece_existence([X_actual,Y_actual], [X_next, Y_next]),
    Play is 0);
    (actual_position(L, Y_next,P),
    \+name(P, [C,_]), check_piece_existence([X_actual, Y_actual], [X_next, Y_next]),
    Play is 1)).


/* Rei preto ou branco */

% testar e realizar a jogada
pieces_rules([C,75], [X_actual,Y_actual], [X_next,Y_next], Play) :-
    ((var(X_next),
    ((X_next is X_actual + 1, Y_next is Y_actual); (X_next is X_actual + 1, (Y_next is Y_actual - 1; Y_next is Y_actual + 1));
    (X_next is X_actual - 1, Y_next is Y_actual); (X_next is X_actual - 1, (Y_next is Y_actual - 1; Y_next is Y_actual + 1));
    (Y_next is Y_actual + 1, X_next is X_actual); (Y_next is Y_actual - 1, X_next is X_actual)),
    X_next > 0, X_next < 9, Y_next > 0, Y_next < 9);
    (\+var(X_next),
    ((X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next), X_dif = Y_dif, X_dif = 1);
    (X_actual = X_next, ((Z is Y_actual + 1, Y_next = Z); (Z is Y_actual - 1, Y_next = Z)));
    (Y_actual = Y_next, ((Z is X_actual + 1, X_next = Z); (Z is X_actual - 1, X_next = Z)))))),
    ASCII is 64 + X_next, char_code(L, ASCII),
    ((\+actual_position(L, Y_next,_),
    Play is 0);
    (actual_position(L, Y_next, P),
    \+name(P, [C,_]),
    Play is 1)).


/* Rainha preta ou branca */

%testar e realizar a jogada
pieces_rules([C,81], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    (   
    (var(X_next), (!, digito(N),
    (
    (X_next is X_actual + N, Y_next is Y_actual);
    (X_next is X_actual + N, (Y_next is Y_actual + N; Y_next is Y_actual - N));
    (X_next is X_actual - N, (Y_next is Y_actual + N; Y_next is Y_actual - N));
    %(((N =< X_actual, X_next is X_actual - N); (N > X_actual, X_next is N - X_actual)), Y_next is Y_actual);
    (X_next is abs(N - X_actual), Y_next is Y_actual);
    (Y_next is N + Y_actual, X_next is X_actual);
    %(((N =< Y_actual, Y_next is Y_actual - N); (N > Y_actual, Y_next is N - Y_actual)), X_next is X_actual);
    (Y_next is abs(N - Y_actual), X_next is X_actual)
    )),
    X_next > 0, X_next < 9, Y_next > 0, Y_next < 9);
    (\+var(X_next),
    (
    (X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next), X_dif = Y_dif);
    (X_actual = X_next); (Y_actual = Y_next)
    )
    )
    ),
    ASCII is 64 + X_next, char_code(L, ASCII),
    (
    (\+actual_position(L, Y_next,_),
    check_piece_existence([X_actual,Y_actual], [X_next,Y_next]),
    Play is 0);
    (actual_position(L, Y_next, P), \+name(P, [C,_]),
    check_piece_existence([X_actual,Y_actual], [X_next, Y_next]),
    Play is 1)
    ).

/* Cavalo preto ou branco */

%testar e realizar a jogada
pieces_rules([C,78], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    (
    (
    var(X_next),
    (
    (X is X_actual + 1, X_next = X, (Y is Y_actual + 2; Y is Y_actual - 2), Y_next = Y);
    (X is X_actual + 2, X_next = X, (Y is Y_actual + 1; Y is Y_actual - 1), Y_next = Y);
    (X is X_actual - 1, X_next = X, (Y is Y_actual + 2; Y is Y_actual - 2), Y_next = Y);
    (X is X_actual - 2, X_next = X, (Y is Y_actual + 1; Y is Y_actual - 1), Y_next = Y)
    ), X_next > 0, X_next < 9, Y_next > 0, Y_next < 9
    );
    (
    \+var(X_next),
    X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next),
    ((X_dif = 1, Y_dif = 2); 
    (X_dif = 2; Y_dif = 1)))
    ),
    ASCII is 64 + X_next, char_code(L, ASCII),
    ((\+actual_position(L, Y_next,_),
    Play is 0);
    (actual_position(L, Y_next, P), \+name(P, [C,_]),
    Play is 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* leitura de inputs */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization(start).

gets :- get0(C),check(C,[]).
gets(L) :- get0(C), check(C,L).

check(-1, L) :- name(STR,L),nl.
check(10,L) :- get_play(L), gets([]).
check(32,L) :- get_play(L), gets([]).
check(C,L) :- append(L,[C],X), gets(X).

check_end(-1,L):- !.
get_content(L) :- get0(C), check(C,L).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* realizar as jogadas dadas no input */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check(C,L) :- write(L), nl,append(L,[C],NL), (check_enter(C,L);check_space(C,L)).

check_enter(10,L) :- get_play(L),get_content([]).

check_space(32,L) :- get_play(L),get_content([]).

get_uppercase_char(C, UC) :-
    (C > 96, C < 123),
    UC is C - 32.

get_number(C, N) :-
    (C > 48, C < 57),
    N is C - 48.

get_play([]).
get_play(L) :-
    analyze_play(L).

analyze_play([A,B]) :- 
    AT is A-96, BT is B-48,n_jogadas(X), NP is X+1 , retractall(n_jogadas(_)),asserta(n_jogadas(NP)),
    Y1 is X mod 2, ((Y1 = 1, COLOR = 87); (Y1 = 0, COLOR = 66)),
    name(P,[COLOR,80]),
    (actual_position(X2,Y,P),char_code(X2,XC),XT is XC-64),
    (pieces_rules([COLOR,80],[XT,Y],[AT,BT],0),!),
    ASCII is A-32, char_code(LETRA,ASCII),
    retract(actual_position(X2,Y,P)),asserta(actual_position(LETRA,BT,P)).

analyze_play([A,B,C]) :-
    BT is B-96, CT is C-48, n_jogadas(X), NP is X+1, retractall(n_jogadas(_)), asserta(n_jogadas(NP)),
    Y1 is X mod 2, ((Y1=1, COLOR = 87); (Y1 = 0, COLOR = 66)),
    name(P,[COLOR,A]),
    (actual_position(X2,Y,P), char_code(X2,XC), XT is XC-64),
    (pieces_rules([COLOR,A],[XT,Y],[BT,CT],0),!),
    ASCII is B-32, char_code(LETRA, ASCII),
    retract(actual_position(X2, Y, P)), asserta(actual_position(LETRA, CT, P)).

analyze_play([A,B,C,D]) :-
    n_jogadas(X), NP is X+1, retractall(n_jogadas(_)), asserta(n_jogadas(NP)),
    Y1 is X mod 2, ((Y1=1, COLOR = 87); (Y1=0, COLOR=66)),
    ((A > 64, A < 91), B = 120, CT is C-96, DT is D-48,
    name(P,[COLOR,A]),
    (actual_position(X2, Y, P), char_code(X2, XC), XT is XC-64),
    (pieces_rules([COLOR,A],[XT,Y],[CT,DT],1),!),
    ASCII is C-32, char_code(LETRA, ASCII),
    retract(actual_position(X2, Y, P)), asserta(actual_position(LETRA, DT, P)));
    ((A > 64, A < 91), \+(B = 120), BT is B-96, CT is C-96, DT is D-96,
    name(P,[COLOR,A]),
    (actual_position(BT, Y, P), char_code(BT, BC), B2 is BC-64),
    (pieces_rules([COLOR,A],[BT,Y],[CT,DT],0),!),
    ASCII is C-32, char_code(LETRA, ASCII),
    retract(actual_position(BT, Y, P)), asserta(actual_position(LETRA, DT, P)));
    ((A>96, A<123), B=120, AT is A-96, CT is C-96, DT is D-48,
    name(P,[COLOR,80]),
    (actual_position(AT,Y,P), char_code(AT, AC), A2 is AC-64),
    (pieces_rules([COLOR,80],[A2,Y],[CT,DT],1),!),
    ASCII is C-32, char_code(LETRA, ASCII),
    retract(actual_position(AT,Y,P)), asserta(actual_position(LETRA, DT, P))).

analyze_play([A,B,C,D,E]) :-
    analyze_play([A,B,C,D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* funções auxiliares */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%retorna se X é um dos inteiros presentes na query
digito(X) :- 
    (
    X = 1;
    X = 2;
    X = 3;
    X = 4;
    X = 5;
    X = 6;
    X = 7
    ).


/*
verifica a existência de uma peça
numa certa posicção do tabuleiro,
quando outra peça se tenta movimentar
para essa posição
*/

% movimento horizontal
check_piece_existence([X_actual,Y_actual], [X_next,Y_actual]) :-
    \+X_actual = X_next,
    ((X_actual > X_next, X is X_actual - 1); (X_next > X_actual, X is X_actual + 1)),
    ASCII is 64 + X, char_code(L, ASCII),
    (X = X_next; \+actual_position(L, Y_actual,_)),
    check_piece_existence([X, Y_actual], [X_next, Y_actual]).

% movimento vertical
check_piece_existence([X_actual,Y_actual], [X_actual, Y_next]) :-
    \+X_actual = X_next,
    ASCII is 64 + X_actual, char_code(L, ASCII),
    ((Y_actual > Y_next, Y is Y_actual - 1); (Y_next > Y_actual, Y is Y_actual + 1)),
    (Y = Y_next; \+actual_position(L, Y,_)),
    check_piece_existence([X_actual,Y], [X_actual,Y_next]).

% movimento diagonal
check_piece_existence([X_actual,Y_actual], [X_next,Y_next]) :-
    X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next), X_dif = Y_dif,
    ((X_actual > X_next, X is X_actual - 1); (X_next > X_actual, X is X_actual + 1)),
    ASCII is 64 + X, char_code(L, ASCII),
    ((Y_actual > Y_next, Y is Y_actual - 1); (Y_next > Y_actual, Y is Y_actual + 1)),
    ((X = X_next, Y = Y_next); \+actual_position(L, Y,_)),
    check_piece_existence([X,Y], [X_next,Y_next]).

% movimento final
check_piece_existence([X_next,Y_next], [X_next,Y_next]).