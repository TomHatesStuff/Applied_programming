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

check_winner([P, P, P | _]) -> true;
check_winner([_, _, _ | Rows]) -> check_winner(Rows);
check_winner([_, _, _ | _]) -> false.

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
