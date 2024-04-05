-module(tic_tac_toe).
-compile(export_all).

% Game state representation
-record(state, {board :: [ [ '_', '_', '_' ], [ '_', '_', '_' ], [ '_', '_', '_' ] ],
                player :: 'X' | 'O' }).

% Function to start the game
start() ->
    State = #state{},
    game_loop(State).

% Game loop
game_loop(State) ->
    display_board(State),
    case check_winner(State) of
        {win, Player} ->
            io:format("Player ~c wins!~n", [Player]);
        draw ->
            io:format("It's a draw!~n");
        continue ->
            try
                NewState = make_move(State),
                game_loop(NewState)
            catch
                throw:invalid_move ->
                    io:format("Invalid move. Please try again.~n"),
                    game_loop(State)
            end
    end.

% Display the game board
display_board(#state{board=Board}) ->
    io:format("~n ~c | ~c | ~c ~n---+---+---~n ~c | ~c | ~c ~n---+---+---~n ~c | ~c | ~c ~n~n",
              lists:flatten(Board)).

% Check if a player has won or if it's a draw
check_winner(#state{board=Board, player=Player}) ->
    % Check rows
    case lists:any(fun(Row) -> Row == [Player, Player, Player] end, Board) of
        true -> {win, Player};
        false ->
            % Check columns
            Transposed = lists:zip(Board),
            case lists:any(fun(Column) -> Column == [Player, Player, Player] end, Transposed) of
                true -> {win, Player};
                false ->
                    % Check diagonals
                    case lists:any(fun(I) -> lists:nth(1, lists:nth(I, Board)) == Player end, [1, 2, 3]) of
                        true -> {win, Player};
                        false ->
                            % Check for draw
                            case lists:all(fun(Row) -> not lists:any(fun(Cell) -> Cell == '_' end, Row) end, Board) of
                                true -> draw;
                                false -> continue
                            end
                    end
            end
    end.

% Make a move
make_move(State) ->
    io:format("Player ~c's turn. Enter move (row,column): ", [State#state.player]),
    {ok, [Row, Column]} = io:fread("~d,~d"),
    case valid_move(State, Row, Column) of
        true ->
            NewBoard = set_cell(State#state.board, Row, Column, State#state.player),
            NewPlayer = case State#state.player of
                            'X' -> 'O';
                            'O' -> 'X'
                        end,
            #state{board=NewBoard, player=NewPlayer};
        false ->
            throw(invalid_move)
    end.

% Check if a move is valid
valid_move(State, Row, Column) when Row >= 1, Row =< 3, Column >= 1, Column =< 3 ->
    lists:nth(Row, lists:nth(Column, State#state.board)) == '_';
valid_move(_, _, _) ->
    false.

% Set a cell on the board to a value
set_cell(Board, Row, Column, Value) ->
    lists:sublist(Board, Row-1) ++
        [lists:sublist(lists:nth(Row, Board), Column-1) ++ [Value] ++ lists:nthtail(Column, lists:nth(Row, Board))] ++
        lists:nthtail(Row, Board).
