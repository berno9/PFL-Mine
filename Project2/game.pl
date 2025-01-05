:- use_module(library(random)).

% Predicado principal para iniciar o jogo
% Este predicado exibe o menu inicial do jogo e solicita ao jogador que escolha uma opção.
% O objetivo é fornecer um ponto de entrada para iniciar ou sair do jogo.
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

% Processa a escolha inicial do jogador.
% Se a opção for válida, ele avança para o próximo passo; caso contrário, exibe uma mensagem de erro.
handle_initial_choice(1) :-
    nl,
    write('*******************************************'), nl,
    write('*         Configuracao do Jogo           *'), nl,
    write('*******************************************'), nl,
    nl,
    write('  Escolha o tamanho do tabuleiro (entre 5 e 10): '), nl,
    read(Size),
    process_size_choice(Size).

handle_initial_choice(2) :-
    nl,
    write('*******************************************'), nl,
    write('*          Obrigado por jogar!            *'), nl,
    write('*******************************************'), nl.

handle_initial_choice(_) :-
    nl,
    write('Opção inválida! Tente novamente.'), nl,
    play.

% Valida o tamanho do tabuleiro escolhido pelo jogador.
% Se o tamanho for válido (entre 5 e 10), avança para a configuração do modo de jogo.
% Caso contrário, solicita uma nova entrada.
process_size_choice(Size) :-
    Size >= 5, Size =< 10,
    !,
    choose_size(Size).
process_size_choice(_) :-
    write('Opção inválida! Tente novamente.'), nl,
    handle_initial_choice(1).

% Exibe o menu de escolha de modo de jogo e solicita ao jogador que escolha uma opção.
choose_size(Size) :-
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
    process_game_type(Size, GameType).

% Valida o tipo de jogo escolhido.
% Se for válido, avança para a escolha do nível de dificuldade (se aplicável).
process_game_type(Size, GameType) :-
    GameType >= 1, GameType =< 4,
    !,
    choose_difficulty(Size, GameType).
process_game_type(Size, _) :-
    write('Opção inválida! Tente novamente.'), nl,
    choose_size(Size).

% Gerencia a escolha do nível de dificuldade ou inicia o jogo diretamente no modo H vs H.
choose_difficulty(Size, GameType) :-
    nl,
    process_game_mode(Size, GameType).

% Define o fluxo baseado no tipo de jogo escolhido.
% No caso de Humano vs Humano (1), o jogo é iniciado diretamente.
% Para outros tipos de jogo, solicita o nível de dificuldade.
process_game_mode(Size, 1) :-
    !,
    nl,
    write('*******************************************'), nl,
    write('*       Iniciando Humano vs Humano       *'), nl,
    write('*******************************************'), nl,
    initial_state([size(Size), player_types(human, human)], GameState),
    game_loop(GameState).
process_game_mode(Size, GameType) :-
    nl,
    write('*******************************************'), nl,
    write('*        Escolha o nivel de dificuldade  *'), nl,
    write('*******************************************'), nl,
    nl,
    write('  1. Nivel 1 (Movimentos Aleatorios)'), nl,
    write('  2. Nivel 2 (Movimentos Inteligentes)'), nl,
    write('  ---------------------------------------'), nl,
    write('  Sua escolha: '),
    read(Level),
    process_difficulty(Size, GameType, Level).

% Valida a escolha do nível de dificuldade.
% Se for válida, configura o jogo. Caso contrário, solicita uma nova entrada
process_difficulty(Size, GameType, Level) :-
    Level >= 1, Level =< 2,
    !,
    setup_game(Size, GameType, Level).
process_difficulty(Size, GameType, _) :-
    write('Nível inválido! Tente novamente.'), nl,
    process_game_mode(Size, GameType).

% Configura o estado inicial do jogo dependendo do modo e do nível de dificuldade selecionados.
% Ele usa `process_game_setup` para criar o estado inicial e, em seguida, inicia o loop principal do jogo.
setup_game(Size, GameType, Level) :-
    process_game_setup(Size, GameType, Level, GameState),
    game_loop(GameState).

% Configura o estado do jogo dependendo do tipo de jogo escolhido.
process_game_setup(Size, 2, Level, GameState) :-
    !,
    write('Iniciando o jogo Humano vs Computador...'), nl,
    initial_state([size(Size), player_types(human, computer(Level))], GameState).
process_game_setup(Size, 3, Level, GameState) :-
    !,
    write('Iniciando o jogo Computador vs Humano...'), nl,
    initial_state([size(Size), player_types(computer(Level), human)], GameState).
process_game_setup(Size, 4, Level, GameState) :-
    !,
    write('Iniciando o jogo Computador vs Computador...'), nl,
    initial_state([size(Size), player_types(computer(Level), computer(Level))], GameState).

% Predicado inicial do estado do jogo
% Configura o estado inicial do jogo baseado nas configurações fornecidas.
% Cria o tabuleiro inicial e calcula as peças de cada jogador.
initial_state(GameConfig, game_state(Board, red, ConfigDetails)) :-
    % Obter configurações do jogo
    member(size(Size), GameConfig),  % Tamanho do tabuleiro
    member(player_types(PlayerRed, PlayerBlue), GameConfig),  % Nomes dos jogadores
    % Gerar o tabuleiro inicial
    generate_board(Size, Board),
    % Configurações adicionais no estado do jogo
    % count_pieces(+Board, +Player, -Count)
    count_pieces(Board, red, RedCount),
    count_pieces(Board, blue, BlueCount),
    ConfigDetails = config(Size, [PlayerRed, PlayerBlue], red(RedCount)-blue(BlueCount)).

% Geração do tabuleiro inicial
% Cria o tabuleiro inicial com padrão alternado para um tamanho específico.
generate_board(Size, Board) :-
    length(Board, Size),  % Garante que o tabuleiro tenha o número correto de linhas
    generate_rows(Size, Size, Board).  % Gera todas as linhas do tabuleiro.

% Geração das linhas do tabuleiro com alternância de padrões
generate_rows(_, 0, []). % Caso base: sem mais linhas para gerar.
generate_rows(Size, N, [Row | Rest]) :-
    N > 0,
    generate_row_pattern(N, Size, Row), % Decide o padrão da linha com base em N.
    N1 is N - 1,
    generate_rows(Size, N1, Rest).

% Determina o padrão da linha com base no número da linha (par ou ímpar).
generate_row_pattern(N, Size, Row) :-
    N mod 2 =:= 0,
    !,
    generate_row(Size, blue(1), red(1), Row).
generate_row_pattern(_, Size, Row) :-
    generate_row(Size, red(1), blue(1), Row).

% Gera uma única linha com padrão alternado
generate_row(Size, First, Second, Row) :-
    length(Row, Size),
    alternating_pattern(Row, First, Second).

% Criar padrão alternado entre dois valores
% Preenche a lista com valores alternados.
alternating_pattern([], _, _).
alternating_pattern([X | Rest], X, Y) :-
    alternating_pattern(Rest, Y, X).

% Ciclo principal do jogo
% O loop principal do jogo. Exibe o estado do jogo, verifica condições de fim e alterna turnos.
game_loop(GameState) :-
    display_game(GameState),
    handle_game_state(GameState, NewGameState),
    game_loop(NewGameState).

% Gerenciar o estado do jogo
% Se o jogo terminou, anuncia o vencedor. Caso contrário, executa o próximo turno.
handle_game_state(GameState, _) :-
    game_over(GameState, Winner),
    announce_winner(Winner).

handle_game_state(GameState, NewGameState) :-
    \+ game_over(GameState, _),
    play_next_turn(GameState, NewGameState).

% Anunciar o vencedor do jogo
% Exibe uma mensagem no terminal indicando quem venceu o jogo.
announce_winner(Winner) :-
    format('O jogo terminou! Vencedor: ~w~n', [Winner]).

% Determinar e jogar o próximo turno
% Identifica o tipo do jogador atual (humano ou computador) e chama o predicado apropriado para realizar o turno.
play_next_turn(GameState, NewGameState) :-
    GameState = game_state(_, CurrentPlayer, Config),
    check_bot_vs_bot_pause(Config),
    determine_player_type(Config, CurrentPlayer, PlayerType),
    play_turn(GameState, PlayerType, NewGameState).

% Pausa o jogo se estiver no modo computador - computador, 
check_bot_vs_bot_pause(Config) :-
    is_bot_vs_bot(Config),
    !,  
    bot_vs_bot_pause.
check_bot_vs_bot_pause(_).

% Verifica se o jogo está no modo computador - computador
is_bot_vs_bot(config(_, [computer(_), computer(_)], _)).

% Pausa para continuar/sair durante jogo de computador - computador
bot_vs_bot_pause :-
    nl,
    write('Prima "s" para continuar para a próxima jogada, ou qualquer outra tecla para sair.'), nl,
    read(Input),
    bot_vs_bot_action(Input).

% Se for s, continua; caso contrário, volta ao início
bot_vs_bot_action(s) :- !. 
bot_vs_bot_action(_) :- play.  


% Determinar o tipo do jogador com base no jogador atual
% Compara o jogador atual (red ou blue) com a configuração para determinar se o jogador é humano ou computador.
determine_player_type(config(_, [PlayerRed, _], _), red, PlayerRed).
determine_player_type(config(_, [_, PlayerBlue], _), blue, PlayerBlue).

% Jogar turno de um jogador
% Dependendo do tipo do jogador (humano ou computador), realiza a jogada correspondente.
play_turn(GameState, human, NewGameState) :-
    interactive_move(GameState, NewGameState).
play_turn(GameState, computer(Level), NewGameState) :-
    choose_move(GameState, Level, Move),
    write('Computador escolheu: '), write(Move), nl,
    move(GameState, Move, NewGameState).

% Verifica se o jogo terminou
% Verifica se todas as peças de um jogador foram capturadas. Se sim, declara o outro jogador como vencedor.
game_over(game_state(Board, _, config(_, _, red(_)-blue(_))), red) :-
    count_pieces(Board, blue, RemainingBlue),
    RemainingBlue =:= 0.

game_over(game_state(Board, _, config(_, _, red(_)-blue(_))), blue) :-
    count_pieces(Board, red, RemainingRed),
    RemainingRed =:= 0.

% Contar as peças restantes de um jogador
% Percorre o tabuleiro e soma o número total de peças do jogador especificado.
count_pieces(Board, Player, Count) :-
    findall(Stack, (member(Row, Board), member(Stack, Row), stack_owner(Stack, Player)), Stacks),
    apply_stack_height(Stacks, Heights),  
    sum_list(Heights, Count).

% Soma os elementos de uma lista
% Calcula a soma de todos os elementos da lista.
sum_list([], 0). 
sum_list([Head | Tail], Sum) :-
    sum_list(Tail, PartialSum),
    Sum is Head + PartialSum.

% Aplica a altura da pilha a cada elemento da lista
% Converte uma lista de pilhas em uma lista de alturas correspondentes.
apply_stack_height([], []).
apply_stack_height([Stack | RestStacks], [Height | RestHeights]) :-
    stack_height(Stack, Height),
    apply_stack_height(RestStacks, RestHeights).

% Obtém a altura de uma pilha
% Retorna a altura de uma pilha, dependendo de sua cor (red ou blue).
stack_height(red(H), H).
stack_height(blue(H), H).

is_empty([]).

% Movimento interativo para jogadores humanos
% Solicita ao jogador humano as coordenadas de origem e destino e tenta realizar o movimento.
interactive_move(GameState, NewGameState) :-
    get_move_inputs(GameState, move(SRow, SCol, TRow, TCol)),
    move(GameState, move(SRow, SCol, TRow, TCol), NewGameState).
interactive_move(GameState, NewGameState) :-
    nl, write('Movimento invalido, tente novamente.'), nl, nl,
    interactive_move(GameState, NewGameState).

% Obter as coordenadas do jogador
% Lê as coordenadas de origem e destino do jogador humano.
get_move_inputs(_, move(SRow, SCol, TRow, TCol)) :-
    write('Prima "e" para reinicar a jogada atual a qualquer momento.'), nl,
    get_single_input('Insira a linha de origem: ', SRow),
    get_single_input('Insira a coluna de origem: ', SCol),
    get_single_input('Insira a linha de destino: ', TRow),
    get_single_input('Insira a coluna de destino: ', TCol).

% Obter uma única entrada do jogador
% Mostra uma mensagem e lê a entrada do jogador. Permite que o jogador cancele com "e".
get_single_input(Message, Value) :-
    write(Message),
    read(Input),
    handle_input(Input, Value).

% Gerenciar entrada do jogador
% Trata a entrada do jogador. Caso seja "e", reinicia a jogada.
handle_input(e, _) :-
    !, fail. 
handle_input(Value, Value).

% Exibir o estado do jogo
% Mostra o estado atual do tabuleiro, o jogador atual e outras informações relevantes.
display_game(game_state(Board, Player, Config)) :-
    nl,
    write('Jogador atual: '), write(Player), nl, nl,
    write('Tabuleiro:'), nl,
    display_board_with_grid(Board),
    nl,
    write('Detalhes da configuracao: '), write(Config), nl,
    value(game_state(Board, Player, Config), Player, Value),
    write('Vantagem: '), write(Value), nl.

% Exibição do tabuleiro com linhas e colunas numeradas
% Mostra o tabuleiro em formato organizado com números de linhas e colunas.
display_board_with_grid(Board) :-
    length(Board, Size),
    write('     '), display_column_labels(Size), nl,
    write('   +'), display_horizontal_line(Size), nl,
    display_board_rows_with_grid(Board, 1).

% Exibir as linhas do tabuleiro
% Exibe as linhas do tabuleiro com os números das linhas.
display_board_rows_with_grid([], _).
display_board_rows_with_grid([Row | Rest], RowNum) :-
    format(' ~w |', [RowNum]),  % Exibe o número da linha
    display_row_with_grid(Row),
    nl,
    length(Row, Size),  % Calcula o número de colunas na linha
    write('   +'), display_horizontal_line(Size), nl,  % Usa o tamanho calculado
    NextRowNum is RowNum + 1,
    display_board_rows_with_grid(Rest, NextRowNum).

% Exibir os elementos de uma linha do tabuleiro
% Mostra os elementos de uma linha, formatados com separadores.
display_row_with_grid([]).
display_row_with_grid([Cell | Rest]) :-
    format_cell(Cell, FormattedCell),
    format(' ~w  |', [FormattedCell]),
    display_row_with_grid(Rest).

% Formatar uma célula do tabuleiro
% Formata as células para exibição, considerando cor e altura.
format_cell(red(Height), FormattedCell) :-
    integer(Height),  % Garante que Height seja um número inteiro
    atom_number(HeightAtom, Height),  % Converte o número em átomo
    atom_concat(' R', HeightAtom, FormattedCell).  % Concatena os valores
format_cell(blue(Height), FormattedCell) :-
    integer(Height),  % Garante que Height seja um número inteiro
    atom_number(HeightAtom, Height),  % Converte o número em átomo
    atom_concat(' B', HeightAtom, FormattedCell).  % Concatena os valores
format_cell(empty, '   ') :- !.
format_cell(_, 'ERR').  % Caso padrão para células inválidas

% Converte um átomo em um número ou um número em um átomo.
atom_number(Atom, Number) :-
    atom(Atom),
    atom_chars(Atom, Chars),
    number_chars(Number, Chars).
atom_number(Atom, Number) :-
    integer(Number),
    number_chars(Number, Chars),
    atom_chars(Atom, Chars).

% Exibir uma linha horizontal
% Exibe uma linha de separação horizontal.
display_horizontal_line(0) :- !, nl. % Caso base: nada a exibir.
display_horizontal_line(Size) :-
    Size > 0,
    write('------+'),
    NewSize is Size - 1,
    display_horizontal_line(NewSize).

% Cria uma lista de números de Low a High (inclusive).
numlist(Low, High, []) :- Low > High, !.
numlist(Low, High, [Low | Rest]) :-
    Low =< High,
    Next is Low + 1,
    numlist(Next, High, Rest).

% Exibir os rótulos das colunas
% Mostra os números das colunas no topo do tabuleiro.
display_column_labels(Size) :-
    numlist(1, Size, ColLabels),
    display_column_labels_recursive(ColLabels).

% Exibir os rótulos das colunas recursivamente
% Mostra cada número de coluna.
display_column_labels_recursive([]).
display_column_labels_recursive([Label | Rest]) :-
    format('   ~w   ', [Label]),
    display_column_labels_recursive(Rest).

% Exibir todos os movimentos
% Mostra no terminal todos os movimentos disponíveis em uma lista.  
display_moves([]).
display_moves([Move | Rest]) :-
    write(Move), nl,
    display_moves(Rest).

% Exibir uma linha do tabuleiro
% Imprime os elementos de uma linha separados por espaços.
print_row([]).
print_row([Cell | Rest]) :-
    format('~w ', [Cell]), 
    print_row(Rest).

% Realizar um movimento no estado atual do jogo
% Verifica se o movimento é válido, executa-o no tabuleiro e atualiza o estado do jogo.
move(game_state(Board, CurrentPlayer, Config), Move, game_state(NewBoard, NextPlayer, NewConfig)) :-
    valid_moves(game_state(Board, CurrentPlayer, Config), ValidMoves),  % Obter todos os movimentos válidos
    member(Move, ValidMoves),                                          % Verificar se o movimento está na lista
    execute_move(Board, Move, NewBoard),                               % Executar o movimento
    update_config(game_state(NewBoard, CurrentPlayer, Config),         % Atualizar a configuração
                  game_state(NewBoard, CurrentPlayer, NewConfig)),
    next_player(CurrentPlayer, NextPlayer).                            % Alternar o jogador

% Alternar o jogador atual
% Alterna entre os jogadores (red e blue).
next_player(red, blue).
next_player(blue, red).

% Obter todos os movimentos válidos
% Retorna todos os movimentos possíveis para o jogador atual.
valid_moves(game_state(Board, _, _), []) :-
    is_empty(Board), !. 
valid_moves(game_state(Board, CurrentPlayer, _), Moves) :-
    setof(
        move(SRow, SCol, TRow, TCol),
        valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)),
        Moves
    ),
    Moves \= [].

% Verificar se um movimento é válido
% Determina se o movimento especificado é válido para o jogador atual.
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
    \+ (SRow == TRow , SCol == TCol),
    valid_destination(Board, SRow, SCol, TRow, TCol). 

% Gera ou verifica números inteiros dentro do intervalo [Low, High].
% Este predicado é usado tanto para gerar todos os números possíveis no intervalo quanto para verificar se um número está contido ne
between(Low, High, Low) :- 
    Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

% Determinar limites do tabuleiro
% Verifica se as coordenadas especificadas estão dentro dos limites do tabuleiro.
within_bounds(Board, Row, Col) :-
    nonvar(Row), nonvar(Col),
    length(Board, Size),
    Row > 0, Row =< Size,
    Col > 0, Col =< Size.

% Determinar o jogador proprietário de uma pilha
% Verifica se a pilha pertence ao jogador especificado.
stack_owner(Stack, Player) :-
    Stack =.. [Player|_]. % Example: red(1), blue(2).

% Retorna o Elemento na posição Index (1-based) da Lista.
nth1(1, [Element | _], Element).  % Caso base: o índice é 1.
nth1(Index, [_ | Rest], Element) :-
    Index > 1,
    NextIndex is Index - 1,
    nth1(NextIndex, Rest, Element).

% Determinar se o destino é válido
% Verifica se o destino está a uma distância válida e é alcançável.
valid_destination(Board, SRow, SCol, TRow, TCol) :-
    nth1(TRow, Board, TargetRow),
    nth1(TCol, TargetRow, _),
    manhattan_distance(SRow, SCol, TRow, TCol, Dist),
    Dist =< 1. 

% Distância Manhattan
% Calcula a distância Manhattan entre a posição de origem e a posição de destino.
manhattan_distance(SRow, SCol, TRow, TCol, Dist) :-
    Dist is abs(SRow - TRow) + abs(SCol - TCol).

% Executa o movimento
% Atualiza o tabuleiro aplicando o movimento especificado.
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

% Determinar o novo valor da célula de destino
% Calcula o novo estado da célula de destino após o movimento. 
determine_new_stack(Stack, empty, Stack) :- !.
determine_new_stack(red(H1), red(H2), red(H3)) :- H3 is H1 + H2, !.  
determine_new_stack(blue(H1), blue(H2), blue(H3)) :- H3 is H1 + H2, !.
determine_new_stack(red(H1), blue(H2), red(H3)) :- 
    H1 >= H2,  
    H3 is H1 - H2 + 1, !.  
determine_new_stack(blue(H1), red(H2), blue(H3)) :- 
    H1 >= H2, 
    H3 is H1 - H2 + 1, !.

% Atualizar uma célula no tabuleiro
% Substitui uma célula no tabuleiro por um novo valor.
update_board(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(OldRow, Col, NewValue, NewRow),
    replace_in_list(Board, Row, NewRow, NewBoard).

% Substituir um elemento em uma lista
% Substitui o elemento na posição especificada por um novo valor.
replace_in_list([_|Rest], 1, NewValue, [NewValue|Rest]).
replace_in_list([Head|Rest], Index, NewValue, [Head|NewRest]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_in_list(Rest, NextIndex, NewValue, NewRest).

% Atualiza a configuração do jogo após um movimento
update_config(game_state(Board, _, config(Size, [PlayerRed, PlayerBlue], red(RedScore)-blue(BlueScore))), 
game_state(Board, _, config(Size, [PlayerRed, PlayerBlue], red(NewRedScore)-blue(NewBlueScore)))) :-
    update_scores(Board, red, RedScore, NewRedScore),
    update_scores(Board, blue, BlueScore, NewBlueScore).

% Atualiza os scores com base no número de peças restantes
update_scores(Board, Player, _, NewScore) :-
    count_pieces(Board, Player, PieceCount),
    NewScore is PieceCount.

% Avaliação do valor do estado do jogo
% Calcula um valor heurístico para o estado do jogo para ajudar o computador a tomar decisões
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

% Obtém o score do jogador com base no estado do jogo
player_score(config(_, _, red(ScoreRed)-blue(_)), red, ScoreRed).
player_score(config(_, _, red(_)-blue(ScoreBlue)), blue, ScoreBlue).

% Calcula a diferença de scores entre o jogador e o oponente
score_difference(GameState, red, ScoreDiff):-
    game_state(_, _, ConfigDetails) = GameState,
    player_score(ConfigDetails, red, PlayerScore),
    player_score(ConfigDetails, blue, OpponentScore),
    ScoreDiff is PlayerScore - OpponentScore.
score_difference(GameState, blue, ScoreDiff):-
    game_state(_, _, ConfigDetails) = GameState,
    player_score(ConfigDetails, blue, PlayerScore),
    player_score(ConfigDetails, red, OpponentScore),
    ScoreDiff is PlayerScore - OpponentScore.

% Calcula a diferença de control do tabuleiro
board_control_difference(GameState, red, ControlDiff) :-
    game_state(Board, _, _) = GameState,
    board_control(Board, red, PlayerCount),
    board_control(Board, blue, OpponentCount),
    ControlDiff is PlayerCount - OpponentCount.
board_control_difference(GameState, blue, ControlDiff) :-
    game_state(Board, _, _) = GameState,
    board_control(Board, blue, PlayerCount),
    board_control(Board, red, OpponentCount),
    ControlDiff is PlayerCount - OpponentCount.

% Conta o número de pilhas controladas por cada jogada
board_control(Board, Player, Count) :-
    findall(Stack, (member(Row, Board), member(Stack, Row), stack_owner(Stack, Player)), Stacks),
    length(Stacks, Count).

% Avalia as oportunidades estratégicas com base nos movimentos válidos
strategic_opportunities(GameState, red, OpportunityDiff):-
    valid_moves(GameState, PlayerMoves),
    length(PlayerMoves, PlayerMovesCount),
    valid_moves(game_state(_, blue, _), OpponentMoves),
    length(OpponentMoves, OpponentMovesCount),
    OpportunityDiff is PlayerMovesCount - OpponentMovesCount.
strategic_opportunities(GameState, blue, OpportunityDiff):-
    valid_moves(GameState, PlayerMoves),
    length(PlayerMoves, PlayerMovesCount),
    valid_moves(game_state(_, red, _), OpponentMoves),
    length(OpponentMoves, OpponentMovesCount),
    OpportunityDiff is PlayerMovesCount - OpponentMovesCount.

% Normaliza o valor calculado para uma escala entre -1 e 1
normalize(Value, NormalizedValue):-
    MaxValue = 100, 
    MinValue = -100,
    NormalizedValue is 2 * ((Value - MinValue) / (MaxValue - MinValue)) - 1.

% Arredonda um número para N casas decimais
round_to_n_decimal_places(Number, N, Rounded) :-
    Factor is 10 ** N,       
    Scaled is Number * Factor,
    RoundedScaled is round(Scaled),  
    Rounded is RoundedScaled / Factor.  

% Escolhe um movimento aleatório no nível 1
choose_move(GameState, 1, Move):-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).  % from random library

% Escolhe o melhor movimento no nível 2 (algoritmo greedy)
choose_move(GameState, 2, BestMove) :-
    valid_moves(GameState, Moves),                % Obtém todos os movimentos válidos
    evaluate_moves(GameState, Moves, ScoredMoves),% Avalia cada movimento
    best_move(ScoredMoves, BestMove).             % Seleciona o melhor movimento

% Avalia todos os movimentos e retorna uma lista com seus valores
evaluate_moves(_, [], []). % Caso base: sem movimentos
evaluate_moves(GameState, [Move | RestMoves], [Move-Score | RestScoredMoves]) :-
    ( 
        move(GameState, Move, NewGameState), 
        GameState = game_state(_, CurrentPlayer, _),
        value(NewGameState, CurrentPlayer, Score)
    ; 
        \+ move(GameState, Move, _),
        Score = -9999
    ),
    evaluate_moves(GameState, RestMoves, RestScoredMoves).

% Seleciona o movimento com o maior valor
best_move([Move-_], Move). % Caso base: apenas um movimento
best_move([Move1-Score1, _-Score2 | Rest], BestMove) :-
    Score1 >= Score2,
    best_move([Move1-Score1 | Rest], BestMove).
best_move([_-Score1, Move2-Score2 | Rest], BestMove) :-
    Score1 < Score2,
    best_move([Move2-Score2 | Rest], BestMove).

