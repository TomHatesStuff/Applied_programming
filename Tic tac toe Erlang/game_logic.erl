% Game Logic Module
-module(game_logic).
-import(output, [display_message/1]).
-export([start_game/0]).

start_game() ->
    Board = board:create_board(),
    play_game(Board).

play_game(Board) ->
    display_message("Current board:"),
    board:display_board(Board),
    case check_winner(Board) of
        true -> display_message("Congratulations! You won!");
        false -> 
            Move = get_player_move(),
            NewBoard = update_board(Board, Move),
            play_game(NewBoard)
    end.

check_winner(Board) ->
    DiagonalWin = check_diagonal(Board),
    ColumnWin = check_columns(Board),
    RowWin = check_rows(Board),
    DiagonalWin orelse ColumnWin orelse RowWin.

check_diagonal(Board) ->
    Diagonal1 = [lists:nth(1, Board), lists:nth(5, Board), lists:nth(9, Board)],
    Diagonal2 = [lists:nth(3, Board), lists:nth(5, Board), lists:nth(7, Board)],
    check_line(Diagonal1) orelse check_line(Diagonal2).

check_columns(Board) ->
    Columns = [[lists:nth(N, Board) || N <- [1, 4, 7]],
                [lists:nth(N, Board) || N <- [2, 5, 8]],
                [lists:nth(N, Board) || N <- [3, 6, 9]]],
    lists:any(fun check_line/1, Columns).

check_rows(Board) ->
    Rows = [lists:sublist(Board, 1, 3),
            lists:sublist(Board, 4, 6),
            lists:sublist(Board, 7, 9)],
    lists:any(fun check_line/1, Rows).

check_line([P, P, P]) when P /= "" -> true;
check_line(_) -> false.

get_player_move() ->
    display_message("Enter your move (1-9): "),
    Input = io:get_line(""),
    list_to_integer(string:strip(Input, right, $\n)).

update_board(Board, Move) ->
    case is_valid_move(Board, Move) of
        true -> lists:sublist(Board, Move-1) ++ ["X"] ++ lists:nthtail(Move, Board);
        false -> display_message("Invalid move! Try again."), update_board(Board, get_player_move())
    end.

is_valid_move(_, Move) when Move >= 1, Move =< 9 -> true;
is_valid_move(_, _) -> false.
