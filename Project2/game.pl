% game.pl

% Predicado inicial do estado do jogo
% initial_state(+GameConfig, -GameState)
% Configura o estado inicial do jogo com base na configuração fornecida.
% GameConfig é uma lista que contém o tamanho do tabuleiro (size(Size)) e os nomes dos jogadores (player_names(PlayerRed, PlayerBlue)).
% GameState contém o tabuleiro, o jogador atual e detalhes adicionais da configuração.
initial_state(GameConfig, game_state(Board, red, ConfigDetails)) :-
    % Obter configurações do jogo
    member(size(Size), GameConfig),  % Tamanho do tabuleiro
    member(player_names(PlayerRed, PlayerBlue), GameConfig),  % Nomes dos jogadores
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

% Predicado principal para iniciar o jogo
% play/0
% Inicia o menu principal do jogo e permite configurar e começar a partida.
play :-
    write('Bem-vindo ao Anaash!'), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Sair'), nl,
    write('Escolha uma opcao: '),
    read(Choice),
    handle_choice(Choice).

% Tratamento da escolha no menu
% handle_choice(+Choice)
% Processa a escolha do menu inicial.
handle_choice(1) :-
    write('Configurando o jogo...'), nl,
    initial_state([size(6), player_names('Alice', 'Bob')], GameState),
    game_loop(GameState).  % Inicia o ciclo principal do jogo

handle_choice(2) :-
    write('Saindo do jogo. Até logo!'), nl.

handle_choice(_) :-
    write('Opção inválida! Tente novamente.'), nl,
    play.

% Ciclo principal do jogo
% game_loop(+GameState)
% Permite movimentos iterativos entre os jogadores até o fim do jogo.
/*game_loop(GameState) :-
    display_game(GameState),
    GameState = game_state(_, CurrentPlayer, _),
    write('Jogador '), write(CurrentPlayer), write(', faça um movimento.'), nl,
    interactive_move(GameState, NewGameState),  % Jogador faz o movimento
    (game_over(NewGameState, Winner) ->  % Verifica se o jogo acabou
        write('O jogo terminou! Vencedor: '), write(Winner), nl
    ;
        game_loop(NewGameState)  % Continua o jogo
    ).*/

game_loop(GameState) :-
    GameState = game_state(_, CurrentPlayer, _),
    write('Jogador '), write(CurrentPlayer), write(', faz um movimento.'), nl,
    interactive_move(GameState, NewGameState),  % Jogador faz o movimento
    (game_over(NewGameState, Winner) ->  % Verifica se o jogo acabou
        write('O jogo terminou! Vencedor: '), write(Winner), nl
    ;
        game_loop(NewGameState)  % Continua o jogo
    ).


% Verifica se o jogo terminou
% game_over(+GameState, -Winner)
% Determina se todas as peças de um jogador foram capturadas.
game_over(game_state(Board, _, config(_, _, red(_)-blue(_))), Winner) :-
    count_pieces(Board, red, RemainingRed),
    count_pieces(Board, blue, RemainingBlue),
    (RemainingRed =:= 0 -> Winner = blue ;
     RemainingBlue =:= 0 -> Winner = red ;
     fail).  % O jogo ainda não terminou

% Conta as peças restantes de um jogador
% count_pieces(+Board, +Player, -Count)
% Conta as peças restantes de um jogador
% count_pieces(+Board, +Player, -Count)
count_pieces(Board, Player, Count) :-
    findall(Stack, (member(Row, Board), member(Stack, Row), stack_owner(Stack, Player)), Stacks),
    apply_stack_height(Stacks, Heights),  % Substitui maplist/3
    sum_list(Heights, Count).

% sum_list(+List, -Sum)
% Soma todos os elementos de uma lista.
sum_list([], 0).  % A soma de uma lista vazia é 0.
sum_list([Head | Tail], Sum) :-
    sum_list(Tail, PartialSum),
    Sum is Head + PartialSum.

% Aplica stack_height a cada elemento de uma lista e retorna os resultados
apply_stack_height([], []).
apply_stack_height([Stack | RestStacks], [Height | RestHeights]) :-
    stack_height(Stack, Height),
    apply_stack_height(RestStacks, RestHeights).


% Exibição do estado atual do jogo
% display_game(+GameState)
% Exibe o tabuleiro atual no terminal.
display_game(game_state(Board, Player, Config)) :-
    write('Jogador atual: '), write(Player), nl,
    write('Tabuleiro:'), nl,
    display_board(Board),
    write('Detalhes da configuracao: '), write(Config), nl.

% Exibição do tabuleiro
% display_board(+Board)
% Exibe o tabuleiro linha por linha, separando os elementos com espaços.
display_board([]).
display_board([Row | Rest]) :-
    print_row(Row),
    nl,  % Pula para a próxima linha
    display_board(Rest).

% Exibição de uma linha do tabuleiro
% print_row(+Row)
% Imprime os elementos de uma linha separados por espaços.
print_row([]).
print_row([Cell | Rest]) :-
    format('~w ', [Cell]),  % Imprime a célula com um espaço
    print_row(Rest).

% move(+GameState, +Move, -NewGameState)
% Executa um movimento, validando se é permitido e retorna o novo estado do jogo.
/*move(game_state(Board, CurrentPlayer, Config), move(SRow, SCol, TRow, TCol), game_state(NewBoard, NextPlayer, Config)) :-
    write('Movimento solicitado: '), write(move(SRow, SCol, TRow, TCol)), nl,
    write('Tabuleiro antes do movimento:'), nl, display_board(Board),
    valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)),
    execute_move(Board, move(SRow, SCol, TRow, TCol), NewBoard),
    next_player(CurrentPlayer, NextPlayer),
    write('Tabuleiro após o movimento:'), nl, display_board(NewBoard).
*/

% move(+GameState, +Move, -NewGameState)
% Executa um movimento, validando se é permitido e retorna o novo estado do jogo.
move(game_state(Board, CurrentPlayer, Config), move(SRow, SCol, TRow, TCol), game_state(NewBoard, NextPlayer, Config)) :-
    valid_move(Board, CurrentPlayer, move(SRow, SCol, TRow, TCol)),
    execute_move(Board, move(SRow, SCol, TRow, TCol), NewBoard),
    next_player(CurrentPlayer, NextPlayer).


% Alterna o jogador atual
% next_player(+CurrentPlayer, -NextPlayer)
next_player(red, blue).
next_player(blue, red).

% Validação de movimento
% valid_move(+Board, +Player, +Move)
% Verifica se o movimento é válido para o jogador atual.
valid_move(Board, Player, move(SRow, SCol, TRow, TCol)) :-
    nth1(SRow, Board, SourceRow),
    nth1(SCol, SourceRow, Stack),
    stack_owner(Stack, Player),
    write('Origem válida: '), write(Stack), nl,
    nth1(TRow, Board, TargetRow),
    nth1(TCol, TargetRow, TargetCell),
    write('Destino valido: '), write(TargetCell), nl,
    valid_destination(Stack, TargetCell),
    write('Destino aprovado: '), write(TargetCell), nl,
    valid_move_type(Board, move(SRow, SCol, TRow, TCol), Stack, TargetCell),
    write('Tipo de movimento valido.'), nl.



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


% Obtém a altura de uma pilha
% stack_height(+Stack, -Height)
stack_height(red(H), H).
stack_height(blue(H), H).

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
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

% Executa o movimento
% execute_move(+Board, +Move, -NewBoard)
execute_move(Board, move(SRow, SCol, TRow, TCol), NewBoard) :-
    nth1(SRow, Board, SourceRow),
    nth1(SCol, SourceRow, Stack),
    % Determina o novo valor da célula destino
    determine_new_stack(Stack, TargetCell, NewStack),
    % Atualiza o tabuleiro
    update_board(Board, SRow, SCol, empty, TempBoard),
    update_board(TempBoard, TRow, TCol, NewStack, NewBoard).

% Calcula o novo valor da célula destino
%determine_new_stack(Stack, empty, Stack).  % Para um movimento posicional.
determine_new_stack(Stack, _, Stack).  % Para um movimento posicional.
determine_new_stack(red(H1), red(H2), red(H3)) :- H3 is H1 + H2.  % Empilhamento.
determine_new_stack(blue(H1), blue(H2), blue(H3)) :- H3 is H1 + H2.
determine_new_stack(_, _, _).  % Outros casos podem ser adicionados, se necessário.


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

% nth1(+Index, +List, -Element)
% Retorna o Elemento na posição Index (1-based) da Lista.
nth1(1, [Element | _], Element).  % Caso base: o índice é 1.
nth1(Index, [_ | Rest], Element) :-
    Index > 1,
    NextIndex is Index - 1,
    nth1(NextIndex, Rest, Element).

/*interactive_move(GameState, NewGameState) :-
    write('Insira a linha de origem: '), read(SRow),
    write('Insira a coluna de origem: '), read(SCol),
    write('Insira a linha de destino: '), read(TRow),
    write('Insira a coluna de destino: '), read(TCol),
    (move(GameState, move(SRow, SCol, TRow, TCol), NewGameState) ->
        write('Movimento realizado com sucesso.'), nl
    ;
        write('Movimento inválido, tente novamente.'), nl,
        interactive_move(GameState, NewGameState)
    ).*/

interactive_move(GameState, NewGameState) :-
    display_game(GameState),  % Exibe o estado atual do tabuleiro
    write('Insira a linha de origem: '), read(SRow),
    write('Insira a coluna de origem: '), read(SCol),
    write('Insira a linha de destino: '), read(TRow),
    write('Insira a coluna de destino: '), read(TCol),
    (move(GameState, move(SRow, SCol, TRow, TCol), NewGameState) ->
        true  % Movimento válido, prossiga
    ;
        write('Movimento invalido, tente novamente.'), nl,
        interactive_move(GameState, NewGameState)  % Tenta novamente
    ).

