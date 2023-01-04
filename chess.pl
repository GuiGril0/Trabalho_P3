%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* inicializar o tabuleiro de jogo */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% peças brancas
initial_position(cell('A', 2), 'WP').
initial_position(cell('B', 2), 'WP').
initial_position(cell('C', 2), 'WP').
initial_position(cell('D', 2), 'WP').
initial_position(cell('E', 2), 'WP').
initial_position(cell('F', 2), 'WP').
initial_position(cell('G', 2), 'WP').
initial_position(cell('H', 2), 'WP').
initial_position(cell('A', 1), 'WT').
initial_position(cell('B', 1), 'WH').
initial_position(cell('C', 1), 'WB').
initial_position(cell('D', 1), 'WQ').
initial_position(cell('E', 1), 'WK').
initial_position(cell('F', 1), 'WB').
initial_position(cell('G', 1), 'WH').
initial_position(cell('H', 1), 'WT').

% peças pretas
initial_position(cell('A', 7), 'BP').
initial_position(cell('B', 7), 'BP').
initial_position(cell('C', 7), 'BP').
initial_position(cell('D', 7), 'BP').
initial_position(cell('E', 7), 'BP').
initial_position(cell('F', 7), 'BP').
initial_position(cell('G', 7), 'BP').
initial_position(cell('H', 7), 'BP').
initial_position(cell('A', 8), 'BT').
initial_position(cell('B', 8), 'BH').
initial_position(cell('C', 8), 'BB').
initial_position(cell('D', 8), 'BQ').
initial_position(cell('E', 8), 'BK').
initial_position(cell('F', 8), 'BB').
initial_position(cell('G', 8), 'BH').
initial_position(cell('H', 8), 'BT').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*  */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(posicao_actual/2).

iniciar :- (initial_position(cell(X, Y),P), \+(posicao_actual(cell(X, Y), P)),
    asserta(posicao_actual(cell(X, Y), P)),iniciar);!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* desenhar o tabuleiro do jogo */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostrar :- iniciar, desenharTabuleiro(2).

desenharTabuleiro(Linha):-
    (Linha=<18,
    ((Y is Linha mod 2,Y=1, escrever_Linha(34),nl);
    (Y is Linha mod 2,Y=0,escrever_Coluna(Linha,0),nl)),
    LinhaN is Linha+1,desenharTabuleiro(LinhaN));!.

escrever_Linha(X):-(X>0,write('-'),escrever_Linha(X-1));!.

escrever_Coluna(2,X):- write('   A   B   C   D   E   F   G   H').

%alterada
escrever_Coluna(Y,X) :-
    (X=<33,
    ((ver_traco(X,0),write('|'),Z is X+1);
    (ver_espacoPeca(X,0),write(' '),Z is X+1);
    (X=0,W is truncate(Y/2-1),write(W),Z is X+1);
    (ver_Peca(Y,X), Z is X+2);
    (write(' '),Z is X+1)),
    escrever_Coluna(Y,Z));!.

ver_traco(Pos,Max):-
    Max=<10,((Y is Pos-4*Max,Y=1,!);(MaxN is Max+1,ver_traco(Pos,MaxN))).

ver_espaco(Pos,Max):-
    Max=<10,(((Y is Pos-4*Max,Y=2,!);
    (Y is Pos-4*Max,Y=4,!));(MaxN is Max+1,ver_espaco(Pos,MaxN))).

ver_letra(Pos,Max):-
    Max=<8,((Y is Pos-4*Max,Y=3,!);(MaxN is Max+1,ver_letra(Pos,MaxN))).

ver_espacoPeca(Pos,Max):-
    Max=<8,(((Y is Pos-4*Max,Y=4,!));
    (MaxN is Max+1,ver_espacoPeca(Pos,MaxN))).

ver_Peca(Y,X):-
    Z is truncate((Y/2) - 1),
    ASCII is truncate(64+(X+2)/4),
    posicao_actual(cell(Letra, Z), Peca),
    char_code(Letra, ASCII),
    write(Peca).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* regras das peças */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* peão branco */

% testar validade da jogada (sem conseqquência no jogo)
pieces_rules([87, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    X_actual = X_next,
    ((Y is Y_actual - 1, Y_next = Y,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+posicao_actual(cell(L, Y_next),_));
    (Y_actual = 2, Y_next = 4,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+posicao_actual(cell(L, Y_next), _),
    Z is Y_next - 1, !, \+posicao_actual(cell(L, Z),_))),
    Play is 0.

% realizar a jogada
pieces_rules([87, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((X is X_actual + 1, X = X_next); 
    (X is X_actual - 1, X = X_next)),
    Y is Y_actual + 1, Y = Y_next,
    ASCII is 64 + X_next, char_code(L, ASCII),
    posicao_actual(cell(L, Y_next), P),
    name(P, [66,_]),
    Play is 1.


/* peão preto */

% testar validade da jogada (sem conseqquência no jogo)
pieces_rules([66, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    X_actual = X_next,
    ((Y is Y_actual - 1, Y_next = Y,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+posicao_actual(cell(L, Y_next),_));
    (Y_actual = 7, Y_next = 5,
    ASCII is 64 + X_next, char_code(L, ASCII),
    \+posicao_actual(cell(L, Y_next),_),
    Z is Y_next - 1, !, \+posicao_actual(cell(L, Z), _))),
    Play is 0.

% realizar a jogada
pieces_rules([66, 80], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((X is X_actual + 1, X = X_next);
    (X is X_actual - 1, X = X_next)),
    Y is Y_actual + 1, Y = Y_next,
    ASCII is 64 + X_next, char_code(L, ASCII),
    posicao_actual(cell(L, Y_next), P),
    name(P, [87,_]),
    Play is 1.


/* torre preta ou branca */

% testar e realizar a jogada
pieces_rules([C, 84], [X_actual, Y_actual], [X_next, Y_next], Play) :-
    ((var(X_next), (!, digito(N), (
    (X_next is X_actual + N, Y_next is Y_actual);
    (((N =< X_actual, X_next is X_actual - N); (N > X_actual, X_next is N - X_actual)), Y_next is Y_actual);
    (((N =< Y_actual, Y_next is Y_actual - N); (N > Y_actual, Y_next is N - Y_actual)), X_next is X_actual))),
    X_next > 0, X_next < 9, 
    Y_next > 0, Y_next < 9);
    (\+var(X_next),
    ((X_actual = X_next); (Y_actual = Y_next)))),
    ASCII is 64 + X_next, char_code(L, ASCII),
    ((\+posicao_actual(cell(L, Y_next),_),
    check_piece_existence([X_actual, Y_actual], [X_next, Y_next]),
    Play is 0);
    (posicao_actual(cell(L, Y_next), P),
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
    ((\+posicao_actual(cell(L, Y_next),_),
    check_piece_existence([X_actual,Y_actual], [X_next, Y_next]),
    Play is 0);
    (posicao_actual(cell(L, Y_next),P),
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
    ((\+posicao_actual(cell(L, Y_next),_),
    Play is 0);
    (posicao_actual(cell(L, Y_next), P),
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
    (\+posicao_actual(cell(L, Y_next),_),
    check_piece_existence([X_actual,Y_actual], [X_next,Y_next]),
    Play is 0);
    (posicao_actual(cell(L, Y_next), P), \+name(P, [C,_]),
    check_piece_existence([X_actual,Y_actual], [X_next, Y_next]),
    Play is 1)
    ).

/* Cavalo preto ou branco */

%testar e realizar a jogada
pieces_rules([C,72], [X_actual, Y_actual], [X_next, Y_next], Play) :-
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
    (
    (X_dif = 1, Y_dif = 2); (X_dif = 2; Y_dif = 1)
    )
    )
    ),
    ASCII is 64 + X_next, char_code(L, ASCII),
    (
    (
    \+posicao_actual(cell(L, Y_next),_),
    Play is 0
    );
    (
    posicao_actual(cell(L, Y_next), P), \+name(P, [C,_]),
    Play is 1
    )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* leitura de inputs */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* realizar as jogadas dadas no input */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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



% define as linhas do tabuleiro
line(1).
line(2).
line(3).
line(4).
line(5).
line(6).
line(7).
line(8).


% define as colunas do tabuleiro
column('A').
column('B').
column('C').
column('D').
column('E').
column('F').
column('G').
column('H').


/* 
define cada célula do tabuleiro, 
sendo que cada célula corresponde a
uma certa linha e a uma certa coluna
*/
cell(X, Y) :- column(X), line(Y).


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
    (X = X_next; \+posicao_actual(cell(L, Y_actual))),
    check_piece_existence([X, Y_actual], [X_next, Y_actual]).

% movimento vertical
check_piece_existence([X_actual,Y_actual], [X_actual, Y_next]) :-
    \+X_actual = X_next,
    ASCII is 64 + X_actual, char_code(L, ASCII),
    ((Y_actual > Y_next, Y is Y_actual - 1); (Y_next > Y_actual, Y is Y_actual + 1)),
    (Y = Y_next; \+posicao_actual(cell(L, Y),_)),
    check_piece_existence([X_actual,Y], [X_actual,Y_next]).

% movimento diagonal
check_piece_existence([X_actual,Y_actual], [X_next,Y_next]) :-
    X_dif is abs(X_actual - X_next), Y_dif is abs(Y_actual - Y_next), X_dif = Y_dif,
    ((X_actual > X_next, X is X_actual - 1); (X_next > X_actual, X is X_actual + 1)),
    ASCII is 64 + X, char_code(L, ASCII),
    ((Y_actual > Y_next, Y is Y_actual - 1); (Y_next > Y_actual, Y is Y_actual + 1)),
    ((X = X_next, Y = Y_next); \+posicao_actual(cell(L, Y),_)),
    check_piece_existence([X,Y], [X_next,Y_next]).

% movimento final
check_piece_existence([X_next,Y_next], [X_next,Y_next]).