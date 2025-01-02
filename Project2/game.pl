:- use_module(library(random)).
% game.pl
% Predicado principal para iniciar o jogo
% play/0
% Inicia o menu principal do jogo e permite configurar e começar a partida.
/*play :-
    write('Bem-vindo ao Anaash!'), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Sair'), nl,
    write('Escolha uma opcao: '),
    read(Choice),
    handle_choice(Choice).*/

% Predicado principal para iniciar o jogo
/*play :-
    write('Bem-vindo ao Anaash!'), nl,
    write('1. Iniciar Jogo'), nl,
    write('2. Sair'), nl,
    write('Escolha uma opcao: '),
    read(Choice),
    handle_initial_choice(Choice).*/

% Predicado principal para iniciar o jogo
play :-
    nl,
    write('*******************************************'), nl,
    write('*           Bem-vindo ao Anaash!         *'), nl,
    write('*******************************************'), nl,
    nl,
    write('  Escolha uma opcao abaixo:'), nl,
    write('  ---------------------------------------'), nl,
    write('  1. Iniciar Jogo'), nl,
    write('  2. Sair'), nl,
    write('  ---------------------------------------'), nl,
    write('  Sua escolha: '),
    read(Choice),
    handle_initial_choice(Choice).

% Menu inicial
/*handle_initial_choice(1) :-
    write('Escolha o modo de jogo:'), nl,
    write('1. Humano vs Humano'), nl,
    write('2. Humano vs Computador'), nl,
    write('3. Computador vs Humano'), nl,
    write('4. Computador vs Computador'), nl,
    write('Escolha o tipo de jogo: '),
    read(GameType),
    (GameType >= 1, GameType =< 4 -> choose_difficulty(GameType)
    ;
        write('Opcao invalida! Tente novamente.'), nl, handle_initial_choice(1)).
handle_initial_choice(2) :-
    write('Saindo do jogo. Até logo!'), nl.
handle_initial_choice(_) :-
    write('Opcao invalida! Tente novamente.'), nl,
    play.*/

% Menu inicial
handle_initial_choice(1) :-
    nl,
    write('*******************************************'), nl,
    write('*         Configuracao do Jogo           *'), nl,
    write('*******************************************'), nl,
    nl,
    write('  Escolha o modo de jogo:'), nl,
    write('  ---------------------------------------'), nl,
    write('  1. Humano vs Humano'), nl,
    write('  2. Humano vs Computador'), nl,
    write('  3. Computador vs Humano'), nl,
    write('  4. Computador vs Computador'), nl,
    write('  ---------------------------------------'), nl,
    write('  Sua escolha: '),
    read(GameType),
    (GameType >= 1, GameType =< 4 -> choose_difficulty(GameType)
    ;
        write('Opção inválida! Tente novamente.'), nl, handle_initial_choice(1)).

handle_initial_choice(2) :-
    nl,
    write('*******************************************'), nl,
    write('*          Obrigado por jogar!           *'), nl,
    write('*******************************************'), nl.

handle_initial_choice(_) :-
    nl,
    write('Opção inválida! Tente novamente.'), nl,
    play.


/*choose_difficulty(GameType) :-
    (GameType = 1 -> 
        % H/H não tem nível de dificuldade
        initial_state([size(6), player_types(human, human)], GameState),
        game_loop(GameState)
    ;
        write('Escolha o nivel de dificuldade:'), nl,
        write('1. Nivel 1 (Movimentos Aleatorios)'), nl,
        write('2. Nivel 2 (Movimentos Inteligentes)'), nl,
        write('Escolha o nivel: '),
        read(Level),
        (Level >= 1, Level =< 2 ->
            setup_game(GameType, Level)
        ;
            write('Nivel inválido! Tente novamente.'), nl, choose_difficulty(GameType))).*/


% Menu para escolher a dificuldade ou iniciar o jogo diretamente no modo H/H
/*choose_difficulty(GameType) :-
    (GameType = 1 -> 
        % H/H não tem nível de dificuldade
        write('Iniciando o jogo Humano vs Humano...'), nl,
        initial_state([size(6), player_types(human, human)], GameState),
        game_loop(GameState)
    ;
        % Para outros modos, solicitar nível de dificuldade
        write('Escolha o nivel de dificuldade:'), nl,
        write('1. Nivel 1 (Movimentos Aleatorios)'), nl,
        write('2. Nivel 2 (Movimentos Inteligentes)'), nl,
        write('Escolha o nivel: '),
        read(Level),
        (Level >= 1, Level =< 2 ->
            setup_game(GameType, Level)
        ;
            write('Nivel inválido! Tente novamente.'), nl, choose_difficulty(GameType))).*/

choose_difficulty(GameType) :-
    nl,
    (GameType = 1 ->
        write('*******************************************'), nl,
        write('*       Iniciando Humano vs Humano       *'), nl,
        write('*******************************************'), nl,
        initial_state([size(6), player_types(human, human)], GameState),
        game_loop(GameState)
    ;
        % Para outros modos, solicitar nível de dificuldade
        write('*******************************************'), nl,
        write('*        Escolha o nivel de dificuldade  *'), nl,
        write('*******************************************'), nl,
        nl,
        write('  1. Nivel 1 (Movimentos Aleatorios)'), nl,
        write('  2. Nivel 2 (Movimentos Inteligentes)'), nl,
        write('  ---------------------------------------'), nl,
        write('  Sua escolha: '),
        read(Level),
        (Level >= 1, Level =< 2 ->
            setup_game(GameType, Level)
        ;
            write('Nível inválido! Tente novamente.'), nl, choose_difficulty(GameType))).

% Configurar o jogo com base no tipo e nível
/*setup_game(GameType, Level) :-
    (GameType = 2 -> initial_state([size(6), player_types(human, computer(Level))], GameState) ;
     GameType = 3 -> initial_state([size(6), player_types(computer(Level), human)], GameState) ;
     GameType = 4 -> initial_state([size(6), player_types(computer(Level), computer(Level))], GameState)),
    game_loop(GameState).*/

setup_game(GameType, Level) :-
    
    (GameType = 2 -> 
        write('Iniciando o jogo Humano vs Computador...'), nl,
        initial_state([size(6), player_types(human, computer(Level))], GameState)
    ;
     GameType = 3 -> 
        write('Iniciando o jogo Computador vs Humano...'), nl,
        initial_state([size(6), player_types(computer(Level), human)], GameState)
    ;
     GameType = 4 -> 
        write('Iniciando o jogo Computador vs Computador...'), nl,
        initial_state([size(6), player_types(computer(Level), computer(Level))], GameState)),
    game_loop(GameState).

% Predicado inicial do estado do jogo
% initial_state(+GameConfig, -GameState)
% Configura o estado inicial do jogo com base na configuração fornecida.
% GameConfig é uma lista que contém o tamanho do tabuleiro (size(Size)) e os nomes dos jogadores (player_names(PlayerRed, PlayerBlue)).
% GameState contém o tabuleiro, o jogador atual e detalhes adicionais da configuração.
initial_state(GameConfig, game_state(Board, red, ConfigDetails)) :-
    % Obter configurações do jogo
    member(size(Size), GameConfig),  % Tamanho do tabuleiro
    member(player_types(PlayerRed, PlayerBlue), GameConfig),  % Nomes dos jogadores
    % Gerar o tabuleiro inicial
    generate_board(Size, Board),
    % Configurações adicionais no estado do jogo
    ConfigDetails = config(Size, [PlayerRed, PlayerBlue], red(0)-blue(0)).

% Geração do tabuleiro inicial
% generate_board(+Size, -Board)
% Cria o tabuleiro inicial com padrão alternado para um tamanho específico.
generate_board(Size, Board) :-
    length(Board, Size),  % Garante que o tabuleiro tenha o número correto de linhas
    generate_rows(Size, Size, Board).  % Gera todas as linhas do tabuleiro.

% Geração das linhas do tabuleiro com alternância de padrões
% generate_rows(+Size, +NumRows, -Rows)
generate_rows(_, 0, []).  % Caso base: sem mais linhas para gerar.
generate_rows(Size, N, [Row | Rest]) :-
    N > 0,
    % Alterna o padrão das linhas: começa com red(1) em linhas ímpares e blue(1) em linhas pares
    (N mod 2 =:= 0 -> generate_row(Size, blue(1), red(1), Row)
                    ; generate_row(Size, red(1), blue(1), Row)),
    N1 is N - 1,  % Decrementa o contador de linhas
    generate_rows(Size, N1, Rest).

% Gera uma única linha com padrão alternado
% generate_row(+Size, +First, +Second, -Row)
generate_row(Size, First, Second, Row) :-
    length(Row, Size),
    alternating_pattern(Row, First, Second).

% Criar padrão alternado entre dois valores
% alternating_pattern(+List, +Value1, +Value2)
% Preenche a lista com valores alternados.
alternating_pattern([], _, _).
alternating_pattern([X | Rest], X, Y) :-
    alternating_pattern(Rest, Y, X).

%%%

/*game_loop(GameState) :-
    GameState = game_state(_, CurrentPlayer, _),
    write('Jogador '), write(CurrentPlayer), write(', faz um movimento.'), nl,
    interactive_move(GameState, NewGameState), 
    (game_over(NewGameState, Winner) -> 
        write('O jogo terminou! Vencedor: '), write(Winner), nl
    ;
        game_loop(NewGameState)  
    ).*/

% Ciclo principal do jogo
game_loop(GameState) :-
    GameState = game_state(_, CurrentPlayer, Config),
    display_game(GameState),
    (game_over(GameState, Winner) -> 
        write('O jogo terminou! Vencedor: '), write(Winner), nl
    ;
        Config = config(_, [PlayerRed, PlayerBlue], _),
        (CurrentPlayer = red -> PlayerType = PlayerRed ; PlayerType = PlayerBlue),
        play_turn(GameState, PlayerType, NewGameState),
        game_loop(NewGameState)
    ).

% Jogar turno de um jogador
play_turn(GameState, human, NewGameState) :-
    interactive_move(GameState, NewGameState).
play_turn(GameState, computer(Level), NewGameState) :-
    choose_move(GameState, Level, Move),
    write('Computador escolheu: '), write(Move), nl,
    move(GameState, Move, NewGameState).

% Verifica se o jogo terminou
% game_over(+GameState, -Winner)
% Determina se todas as peças de um jogador foram capturadas.
game_over(game_state(Board, _, config(_, _, red(_)-blue(_))), Winner) :-
    count_pieces(Board, red, RemainingRed),
    count_pieces(Board, blue, RemainingBlue),
    (RemainingRed =:= 0 -> Winner = blue ;
     RemainingBlue =:= 0 -> Winner = red ;
     fail).  

% Conta as peças restantes de um jogador
% count_pieces(+Board, +Player, -Count)
% Conta as peças restantes de um jogador
% count_pieces(+Board, +Player, -Count)
count_pieces(Board, Player, Count) :-
    findall(Stack, (member(Row, Board), member(Stack, Row), stack_owner(Stack, Player)), Stacks),
    apply_stack_height(Stacks, Heights),  
    sum_list(Heights, Count).

% sum_list(+List, -Sum)
% Soma todos os elementos de uma lista.
sum_list([], 0). 
sum_list([Head | Tail], Sum) :-
    sum_list(Tail, PartialSum),
    Sum is Head + PartialSum.

% Aplica stack_height a cada elemento de uma lista e retorna os resultados
apply_stack_height([], []).
apply_stack_height([Stack | RestStacks], [Height | RestHeights]) :-
    stack_height(Stack, Height),
    apply_stack_height(RestStacks, RestHeights).

% Obtém a altura de uma pilha
% stack_height(+Stack, -Height)
stack_height(red(H), H).
stack_height(blue(H), H).

is_empty([]).

interactive_move(GameState, NewGameState) :-
    %display_game(GameState),
    write('Insira a linha de origem: '), read(SRow),
    write('Insira a coluna de origem: '), read(SCol),
    write('Insira a linha de destino: '), read(TRow),
    write('Insira a coluna de destino: '), read(TCol),
    (move(GameState, move(SRow, SCol, TRow, TCol), NewGameState) ->
        true 
    ;
        write('Movimento invalido, tente novamente.'), nl,
        interactive_move(GameState, NewGameState)  
    ).

% display_game(+GameState)
% Exibe o tabuleiro atual no terminal de maneira amigável para os jogadores.
display_game(game_state(Board, Player, Config)) :-
    nl,
    write('Jogador atual: '), write(Player), nl, nl,
    write('Tabuleiro:'), nl,
    display_board_with_grid(Board),
    nl,
    write('Detalhes da configuracao: '), write(Config), nl,
    value(game_state(Board, Player, Config), Player, Value),
    write('Vantagem: '), write(Value), nl.

% Exibição do tabuleiro
% display_board(+Board)
% Exibe o tabuleiro linha por linha, separando os elementos com espaços.
display_board_with_grid(Board) :-
    length(Board, Size),
    write('     '), display_column_labels(Size), nl,
    write('   +'), display_horizontal_line(Size), nl,
    display_board_rows_with_grid(Board, 1).

% display_board_rows_with_grid(+Board, +RowNum)
% Exibe cada linha do tabuleiro com separadores e números das linhas.
display_board_rows_with_grid([], _).
display_board_rows_with_grid([Row | Rest], RowNum) :-
    format(' ~w | ', [RowNum]),  % Exibe o número da linha
    display_row_with_grid(Row),
    nl,
    length(Row, Size),  % Calcula o número de colunas na linha
    write('   +'), display_horizontal_line(Size), nl,  % Usa o tamanho calculado
    NextRowNum is RowNum + 1,
    display_board_rows_with_grid(Rest, NextRowNum).

% Exibe os elementos de uma linha separados por linhas verticais.
display_row_with_grid([]).
display_row_with_grid([Cell | Rest]) :-
    format_cell(Cell, FormattedCell),
    format(' ~w | ', [FormattedCell]),
    display_row_with_grid(Rest).

format_cell(empty, '  ') :- !.
format_cell(red(Height), FormattedCell) :-
    integer(Height),  % Garante que Height seja um número inteiro
    atom_number(HeightAtom, Height),  % Converte o número em átomo
    atom_concat(' R', HeightAtom, FormattedCell).  % Concatena os valores
format_cell(blue(Height), FormattedCell) :-
    integer(Height),  % Garante que Height seja um número inteiro
    atom_number(HeightAtom, Height),  % Converte o número em átomo
    atom_concat(' B', HeightAtom, FormattedCell).  % Concatena os valores
format_cell(_, 'ERR').  % Caso padrão para células inválidas

% atom_number(+Atom, -Number)
% Converte um átomo em um número ou um número em um átomo.
atom_number(Atom, Number) :-
    (atom(Atom) -> atom_chars(Atom, Chars), number_chars(Number, Chars)
    ; integer(Number) -> number_chars(Number, Chars), atom_chars(Atom, Chars)).


% display_horizontal_line(+Size)
% Exibe uma linha horizontal de separação usando recursão.
display_horizontal_line(0) :- !, nl. % Caso base: nada a exibir.
display_horizontal_line(Size) :-
    Size > 0,
    write('------+'),
    NewSize is Size - 1,
    display_horizontal_line(NewSize).


% numlist(+Low, +High, -List)
% Cria uma lista de números de Low a High (inclusive).
numlist(Low, High, []) :- Low > High, !.
numlist(Low, High, [Low | Rest]) :-
    Low =< High,
    Next is Low + 1,
    numlist(Next, High, Rest).


% display_column_labels(+Size)
% Exibe os rótulos das colunas na parte superior do tabuleiro.
display_column_labels(Size) :-
    numlist(1, Size, ColLabels),
    display_column_labels_recursive(ColLabels).

% display_column_labels_recursive(+Labels)
% Exibe os rótulos das colunas recursivamente.
display_column_labels_recursive([]).
display_column_labels_recursive([Label | Rest]) :-
    format('   ~w   ', [Label]),
    display_column_labels_recursive(Rest).

    
display_moves([]).
display_moves([Move | Rest]) :-
    write(Move), nl,
    display_moves(Rest).

% Exibição de uma linha do tabuleiro
% print_row(+Row)
% Imprime os elementos de uma linha separados por espaços.
print_row([]).
print_row([Cell | Rest]) :-
    format('~w ', [Cell]), 
    print_row(Rest).


%%%

/*move(game_state(Board, CurrentPlayer, Config), Move, game_state(NewBoard, NextPlayer, NewConfig)) :-
    % Geração de todos os movimentos válidos
    valid_moves(game_state(Board, CurrentPlayer, Config), ListOfMoves), 
    % Verificar se a jogada atual está nessa lista
    member(Move, ListOfMoves), 
    % Execução do movimento no tabuleiro
    execute_move(Board, Move, NewBoard), 
    % Atualizar a configuração
    update_config(game_state(NewBoard, CurrentPlayer, Config), game_state(NewBoard, CurrentPlayer, NewConfig)),
    % Alternar o jogador
    next_player(CurrentPlayer, NextPlayer). */

move(game_state(Board, CurrentPlayer, Config), Move, game_state(NewBoard, NextPlayer, NewConfig)) :-
    valid_moves(game_state(Board, CurrentPlayer, Config), ValidMoves),  % Obter todos os movimentos válidos
    member(Move, ValidMoves),                                          % Verificar se o movimento está na lista
    execute_move(Board, Move, NewBoard),                               % Executar o movimento
    update_config(game_state(NewBoard, CurrentPlayer, Config),         % Atualizar a configuração
                  game_state(NewBoard, CurrentPlayer, NewConfig)),
    next_player(CurrentPlayer, NextPlayer).                            % Alternar o jogador


% Alterna o jogador atual
% next_player(+CurrentPlayer, -NextPlayer)
next_player(red, blue).
next_player(blue, red).

%% valid_moves
valid_moves(game_state(Board, _, _), []) :-
    is_empty(Board), !. 
valid_moves(game_state(Board, CurrentPlayer, Config), Moves) :-
    setof(
        move(SRow, SCol, TRow, TCol),
        valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)),
        Moves
    ).

valid_moves(game_state(Board, CurrentPlayer, _), Moves) :-
    findall(move(SRow, SCol, TRow, TCol),
            valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)),
            Moves),
    (Moves = [] -> fail ; true). % Retorna falha se não houver movimentos válidos


valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)) :-
    length(Board, Size), 
    between(1, Size, SRow), 
    between(1, Size, SCol), 
    within_bounds(Board, SRow, SCol), 
    nth1(SRow, Board, SourceRow),
    nth1(SCol, SourceRow, Stack),
    stack_owner(Stack, CurrentPlayer), 
    between(1, Size, TRow), 
    between(1, Size, TCol), 
    valid_destination(Board, SRow, SCol, TRow, TCol). 

between(Low, High, Low) :- 
    Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

within_bounds(Board, Row, Col) :-
    nonvar(Row), nonvar(Col),
    length(Board, Size),
    Row > 0, Row =< Size,
    Col > 0, Col =< Size.

stack_owner(Stack, Player) :-
    Stack =.. [Player|_]. % Example: red(1), blue(2).

% nth1(+Index, +List, -Element)
% Retorna o Elemento na posição Index (1-based) da Lista.
nth1(1, [Element | _], Element).  % Caso base: o índice é 1.
nth1(Index, [_ | Rest], Element) :-
    Index > 1,
    NextIndex is Index - 1,
    nth1(NextIndex, Rest, Element).

valid_destination(Board, SRow, SCol, TRow, TCol) :-
    nth1(TRow, Board, TargetRow),
    nth1(TCol, TargetRow, TargetCell),
    %is_empty(TargetCell),
    manhattan_distance(SRow, SCol, TRow, TCol, Dist),
    Dist =< 1. 

manhattan_distance(SRow, SCol, TRow, TCol, Dist) :-
    Dist is abs(SRow - TRow) + abs(SCol - TCol).

% Executa o movimento
% execute_move(+Board, +Move, -NewBoard)
execute_move(Board, move(SRow, SCol, TRow, TCol), NewBoard) :-
    nth1(SRow, Board, SourceRow),
    nth1(SCol, SourceRow, Stack),
    nth1(TRow, Board, TargetRow),                
    nth1(TCol, TargetRow, TargetCell),
    % Determina o novo valor da célula destino
    determine_new_stack(Stack, TargetCell, NewStack),
    % Atualiza o tabuleiro
    update_board(Board, SRow, SCol, empty, TempBoard),
    update_board(TempBoard, TRow, TCol, NewStack, NewBoard).

% Calcula o novo valor da célula destino
%determine_new_stack(Stack, empty, Stack).  
determine_new_stack(Stack, empty, Stack) :- !.
determine_new_stack(red(H1), red(H2), red(H3)) :- H3 is H1 + H2, !.  
determine_new_stack(blue(H1), blue(H2), blue(H3)) :- H3 is H1 + H2, !.
determine_new_stack(red(H1), blue(H2), red(H3)) :- 
    H1 >= H2,  
    H3 is H1 - H2 + 1, !.  
determine_new_stack(blue(H1), red(H2), blue(H3)) :- 
    H1 >= H2, 
    H3 is H1 - H2 + 1, !.

% Atualiza uma célula do tabuleiro
% update_board(+Board, +Row, +Col, +NewValue, -NewBoard)
update_board(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, NewValue, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

% Substitui um elemento em uma lista
% replace_in_list(+List, +Index, +NewValue, -NewList)
replace_in_list([_|Rest], 1, NewValue, [NewValue|Rest]).
replace_in_list([Head|Rest], Index, NewValue, [Head|NewRest]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_in_list(Rest, NextIndex, NewValue, NewRest).

update_config(game_state(Board, CurrentPlayer, config(Size, [PlayerRed, PlayerBlue], red(RedScore)-blue(BlueScore))), 
game_state(Board, NextPlayer, config(Size, [PlayerRed, PlayerBlue], red(NewRedScore)-blue(NewBlueScore)))) :-
    update_scores(Board, red, RedScore, NewRedScore),
    update_scores(Board, blue, BlueScore, NewBlueScore).

update_scores(Board, Player, OldScore, NewScore) :-
    count_pieces(Board, Player, PieceCount),
    NewScore is PieceCount.

%%
%value(+GameState, +Player, -Value)
/*value(GameState, Player, Value):-
    score_difference(GameState, Player, ScoreDiff),
    board_control_difference(GameState, Player, ControlDiff),
    strategic_opportunities(GameState, Player, OpportunityDiff),
    WeightScore = 0.5, 
    WeightControl = 0.3,
    WeightOpportunities = 0.2,
    RawValue is (WeightScore * ScoreDiff) + (WeightControl * ControlDiff) + (WeightOpportunities * OpportunityDiff),
    normalize(RawValue, NormalizedValue),
    round_to_n_decimal_places(NormalizedValue, 3, Value).*/

value(game_state([], _, _), _, 0) :- !.  % Valor neutro para tabuleiros vazios
value(GameState, Player, Value) :-
    score_difference(GameState, Player, ScoreDiff),
    board_control_difference(GameState, Player, ControlDiff),
    strategic_opportunities(GameState, Player, OpportunityDiff),
    WeightScore = 0.5, 
    WeightControl = 0.3,
    WeightOpportunities = 0.2,
    RawValue is (WeightScore * ScoreDiff) + (WeightControl * ControlDiff) + (WeightOpportunities * OpportunityDiff),
    normalize(RawValue, NormalizedValue),
    round_to_n_decimal_places(NormalizedValue, 3, Value).


% metrics for value
player_score(config(_, _, red(ScoreRed)-blue(ScoreBlue)), red, ScoreRed).
player_score(config(_, _, red(ScoreRed)-blue(ScoreBlue)), blue, ScoreBlue).

score_difference(GameState, Player, ScoreDiff):-
    game_state(_, _, ConfigDetails) = GameState,
    player_score(ConfigDetails, Player, PlayerScore),
    (Player = red -> Opponent = blue ; Opponent = red),
    player_score(ConfigDetails, Opponent, OpponentScore),
    ScoreDiff is PlayerScore - OpponentScore.

board_control_difference(GameState, Player, ControlDiff) :-
    game_state(Board, _, _) = GameState,
    board_control(Board, Player, PlayerCount),
    (Player = red -> Opponent = blue ; Opponent = red),
    board_control(Board, Opponent, OpponentCount),
    ControlDiff is PlayerCount - OpponentCount.

board_control(Board, Player, Count) :-
    findall(Stack, (member(Row, Board), member(Stack, Row), stack_owner(Stack, Player)), Stacks),
    length(Stacks, Count).

strategic_opportunities(GameState, Player, OpportunityDiff):-
    valid_moves(GameState, PlayerMoves),
    length(PlayerMoves, PlayerMovesCount),
    (Player = red -> Opponent = blue ; Opponent = red),
    valid_moves(game_state(_, Opponent, _), OpponentMoves),
    length(OpponentMoves, OpponentMovesCount),
    OpportunityDiff is 5 - OpponentMovesCount.

% normalize the value for human understanding
normalize(Value, NormalizedValue):-
    MaxValue = 100, 
    MinValue = -100,
    NormalizedValue is 2 * ((Value - MinValue) / (MaxValue - MinValue)) - 1.

round_to_n_decimal_places(Number, N, Rounded) :-
    Factor is 10 ** N,       
    Scaled is Number * Factor,
    RoundedScaled is round(Scaled),  
    Rounded is RoundedScaled / Factor.  


choose_move(GameState, 1, Move):-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).  % from random library

% Predicado para escolher o movimento no nível 2 (algoritmo greedy)
choose_move(GameState, 2, BestMove) :-
    valid_moves(GameState, Moves),                % Obtém todos os movimentos válidos
    evaluate_moves(GameState, Moves, ScoredMoves),% Avalia cada movimento
    best_move(ScoredMoves, BestMove).             % Seleciona o melhor movimento

% Avalia todos os movimentos e retorna uma lista de movimentos com seus valores
/*evaluate_moves(_, [], []).                        % Caso base: sem movimentos
evaluate_moves(GameState, [Move | RestMoves], [Move-Score | RestScoredMoves]) :-
    move(GameState, Move, NewGameState),          % Simula o movimento
    GameState = game_state(_, CurrentPlayer, _),  % Obtém o jogador atual
    value(NewGameState, CurrentPlayer, Score),    % Avalia o estado resultante
    evaluate_moves(GameState, RestMoves, RestScoredMoves). % Avalia o restante*/

evaluate_moves(_, [], []). % Caso base: sem movimentos
evaluate_moves(GameState, [Move | RestMoves], [Move-Score | RestScoredMoves]) :-
    ( move(GameState, Move, NewGameState) ->  % Tenta simular o movimento
        GameState = game_state(_, CurrentPlayer, _),  
        value(NewGameState, CurrentPlayer, Score)   % Avalia o estado resultante
    ;
        Score = -9999  % Atribui um valor baixo para movimentos inválidos
    ),
    evaluate_moves(GameState, RestMoves, RestScoredMoves).


% Seleciona o movimento com o maior valor
best_move([Move-Score], Move) :- !.               % Caso base: apenas um movimento
best_move([Move1-Score1, Move2-Score2 | Rest], BestMove) :-
    (Score1 >= Score2 ->                          % Compara os valores
        best_move([Move1-Score1 | Rest], BestMove)
    ;
        best_move([Move2-Score2 | Rest], BestMove)
    ).


/*choose_move(GameState, 2, Move):-
    valid_moves(GameState, Move),
    evaluate_moves(GameState, Moves, ScoredMoves),
    best_move(ScoredMoves, BestMove).

evaluate_moves(_, [], []).
evaluate_moves(GameState, [Move | RestMoves], [Move-Score | RestScoredMoves]):-
    game_state(Board, Player, Config) = GameState,
    execute_move(Board, Move, NewBoard),
    update_config(game_state(NewBoard, Player, Config), game_state(NewBoard, Player, NewConfig)),
    value(game_state(NewBoard, Player, NewConfig), Player, Score),
    evaluate_moves(GameState, RestMoves, RestScoredMoves).

best_move([Move-Score], Move):- !.
best_move([Move1-Score1, Move2-Score2 | Rest], BestMove):-
    (Score1 >= Score2 -> 
        best_move([Move1-Score1 | Rest], BestMove)  
    ;
        best_move([Move2-Score2 | Rest], BestMove)
    ).*/









% Exibição do estado atual do jogo
% display_game(+GameState)
% Exibe o tabuleiro atual no terminal.
/*display_game(game_state(Board, Player, Config)) :-
    write('Jogador atual: '), write(Player), nl,
    write('Tabuleiro:'), nl,
    display_board(Board),
    write('Detalhes da configuracao: '), write(Config), nl,
    value(game_state(Board, Player, Config), Player, Value),
    write('Vantagem: '), write(Value), nl.*/

/*display_board([]).
display_board([Row | Rest]) :-
    print_row(Row),
    nl,  
    display_board(Rest).*/

% atom_concat(+Atom1, +Atom2, -Result)
% Concatena os átomos Atom1 e Atom2 em Result.
/*atom_concat(Atom1, Atom2, Result) :-
    atom(Atom1),        % Verifica se Atom1 é um átomo
    atom(Atom2),        % Verifica se Atom2 é um átomo
    atom_chars(Atom1, Chars1),  % Divide Atom1 em caracteres
    atom_chars(Atom2, Chars2),  % Divide Atom2 em caracteres
    append(Chars1, Chars2, CharsResult),  % Concatena as listas de caracteres
    atom_chars(Result, CharsResult).  % Converte de volta para átomo*/

/*between(Low, High, Low) :- 
    Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

valid_moves(game_state(Board, Player, _), ListOfMoves):-
    length(Board, Size),
    findall(move(SRow, SCol, TRow, TCol),
        (
            between(1, Size, SRow),
            nth1(SRow, Board, SourceRow),
            between(1, Size, SCol),
            nth1(SCol, SourceRow, Stack),  
            stack_owner(Stack, Player),
            between(1, Size, TRow),   
            nth1(TRow, Board, TargetRow),
            between(1, Size, TCol),
            nth1(TCol, TargetRow, TargetCell),  
            valid_destination(Stack, TargetCell),
            valid_move_type(Board, move(SRow, SCol, TRow, TCol), Stack, TargetCell)
        ),
        ListOfMoves).


% Identifica o dono de uma pilha
% stack_owner(+Stack, +Player)
stack_owner(red(_), red).
stack_owner(blue(_), blue).

% Verifica se o destino é válido
% valid_destination(+Stack, +TargetCell)
valid_destination(Stack, empty).  % Destino vazio é válido para movimento posicional.
valid_destination(Stack, TargetCell) :-
    % Empilhamento: amigo e de altura igual ou maior.
    stack_owner(Stack, Player),
    stack_owner(TargetCell, Player),
    stack_height(TargetCell, TargetHeight),
    stack_height(Stack, StackHeight),
    TargetHeight >= StackHeight.
valid_destination(Stack, TargetCell) :-
    % Captura: inimigo e de altura igual ou menor.
    stack_owner(Stack, Player),
    stack_owner(TargetCell, Enemy),
    Player \= Enemy,
    stack_height(TargetCell, TargetHeight),
    stack_height(Stack, StackHeight),
    StackHeight >= TargetHeight.


% Valida o tipo de movimento (posicional, empilhamento ou captura)
% valid_move_type(+Board, +Move, +Stack, +TargetCell)
valid_move_type(Board, move(SRow, SCol, TRow, TCol), Stack, empty) :-
    % Movimento posicional: Manhattan distance deve diminuir
    manhattan_distance(Board, SRow, SCol, TRow, TCol, Stack, DistBefore, DistAfter),
    DistAfter < DistBefore.
valid_move_type(_, _, _, _).  % Para empilhamento e captura, já validado.

% Calcula a distância Manhattan entre uma pilha e a mais próxima
% manhattan_distance(+Board, +SRow, +SCol, +TRow, +TCol, +Stack, -DistBefore, -DistAfter)
manhattan_distance(Board, SRow, SCol, TRow, TCol, Stack, DistBefore, DistAfter) :-
    find_closest_stack(Board, SRow, SCol, Stack, ClosestRow, ClosestCol),
    DistBefore is abs(SRow - ClosestRow) + abs(SCol - ClosestCol),
    DistAfter is abs(TRow - ClosestRow) + abs(TCol - ClosestCol),
    write('Distância antes: '), write(DistBefore), write(', depois: '), write(DistAfter), nl.

% find_closest_stack(+Board, +SRow, +SCol, +Stack, -ClosestRow, -ClosestCol)
% Encontra a pilha mais próxima na mesma linha ou coluna (não diagonal).
find_closest_stack(Board, SRow, SCol, Stack, ClosestRow, ClosestCol) :-
    findall((Dist, Row, Col),
        (nth1(Row, Board, Line), 
         nth1(Col, Line, Cell),
         Cell \= empty,  % Deve ser uma célula ocupada
         Cell \= Stack,  % Não considerar a célula atual
         manhattan(SRow, SCol, Row, Col, Dist)),  % Calcula a distância
        Distances),
    sort(Distances, SortedDistances),  % Ordena por menor distância
    SortedDistances = [(MinDist, ClosestRow, ClosestCol) | _],  % Pega o mais próximo
    write('Distância mínima encontrada: '), write(MinDist), nl.

% manhattan(+Row1, +Col1, +Row2, +Col2, -Distance)
% Calcula a distância Manhattan entre duas posições.
manhattan(Row1, Col1, Row2, Col2, Distance) :-
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).*/


/*% Tratamento da escolha no menu
% handle_choice(+Choice)
% Processa a escolha do menu inicial.
handle_choice(1) :-
    write('Configurando o jogo...'), nl,
    initial_state([size(6), player_names('Alice', 'Bob')], GameState),
    game_loop(GameState). % Inicia o ciclo principal do jogo

handle_choice(2) :-
    write('Saindo do jogo. Até logo!'), nl.

handle_choice(_) :-
    write('Opcao invalida! Tente novamente.'), nl,
    play.*/