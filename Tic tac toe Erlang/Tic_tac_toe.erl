-module(tic_tac_toe).
-export([start/0]).

% Function to start the game
start() ->
    Board = init_board(),
    play(Board, x).

% Function to initialize the game board
init_board() ->
    [[empty,empty,empty],
     [empty,empty,empty],
     [empty,empty,empty]].

% Function to display the game board
display_board(Board) ->
    io:format("~n~c | ~c | ~c~n", [display_cell(Board, 1, 1), display_cell(Board, 1, 2), display_cell(Board, 1, 3)]),
    io:format("---------~n"),
    io:format("~c | ~c | ~c~n", [display_cell(Board, 2, 1), display_cell(Board, 2, 2), display_cell(Board, 2, 3)]),
    io:format("---------~n"),
    io:format("~c | ~c | ~c~n~n", [display_cell(Board, 3, 1), display_cell(Board, 3, 2), display_cell(Board, 3, 3)]).

% Function to display a cell on the board
display_cell(Board, Row, Col) ->
    case lists:nth(Row, lists:nth(Col, Board)) of
        x -> $X;
        o -> $O;
        _ -> $_
    end.

% Function to start the game loop
play(Board, Player) ->
    display_board(Board),
    case check_win(Board) of
        {win, Player} ->
            io:format("Player ~c wins!~n", [Player]);
        {win, _} ->
            io:format("It's a draw!~n");
        continue ->
            NewBoard = make_move(Board, Player),
            NextPlayer = case Player of
                x -> o;
                o -> x
            end,
            play(NewBoard, NextPlayer)
    end.

% Function to make a move
make_move(Board, Player) ->
    io:format("Player ~c's turn. Enter row (1-3) and column (1-3) separated by a space: ", [Player]),
    {ok, [Row, Col]} = io:fread("", "~d ~d"),
    case is_valid_move(Board, Row, Col) of
        true ->
            NewBoard = set_cell(Board, Row, Col, Player),
            NewBoard;
        false ->
            io:format("Invalid move. Please try again.~n"),
            make_move(Board, Player)
    end.

% Function to check if a move is valid
is_valid_move(Board, Row, Col) ->
    case lists:nth(Row, lists:nth(Col, Board)) of
        _ -> true;
        _ -> false
    end.

% Function to set a cell with a player's move
set_cell(Board, Row, Col, Value) ->
    lists:sublist(Board, Row-1) ++
    [lists:sublist(lists:nth(Row, Board), Col-1) ++
    [Value | tl(lists:nth(Row, Board, Col))] |
    tl(Board)].

% Function to check for a win
check_win(Board) ->
    case check_rows(Board) of
        {win, Player} -> {win, Player};
        _ ->
            case check_columns(Board) of
                {win, Player} -> {win, Player};
                _ ->
                    case check_diagonals(Board) of
                        {win, Player} -> {win, Player};
                        _ ->
                            case check_draw(Board) of
                                true -> {win, empty};
                                false -> continue
                            end
                    end
            end
    end.

% Function to check rows for a win
check_rows(Board) ->
    check_lines(Board).

% Function to check columns for a win
check_columns(Board) ->
    TransposedBoard = lists:zip(Board),
    check_lines(TransposedBoard).

% Function to check diagonals for a win
check_diagonals(Board) ->
    Diagonals = [[lists:nth(Row, lists:nth(Row, Board)) || Row <- lists:seq(1, 3)],
                 [lists:nth(Row, lists:nth(4-Row, Board)) || Row <- lists:seq(1, 3)]],
    check_lines(Diagonals).

% Function to check lines for a win
check_lines([Line | Rest]) ->
    case Line of
        [x,x,x] -> {win, x};
        [o,o,o] -> {win, o};
        _ -> check_lines(Rest)
    end.

% Function to check for a draw
check_draw(Board) ->
    not lists:any(fun(Row) -> lists:any(fun(Cell) -> Cell == empty end, Row) end, Board).
