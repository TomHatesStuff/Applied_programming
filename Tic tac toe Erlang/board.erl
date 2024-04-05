% Game Board Module
-module(board).
-export([create_board/0, display_board/1]).

create_board() -> [1, 2, 3, 4, 5, 6, 7, 8, 9].

display_board(Board) ->
    [A, B, C, D, E, F, G, H, I] = Board,
    output:display_message("-------------"),
    output:display_message("| " ++ display_cell(A) ++ " | " ++ display_cell(B) ++ " | " ++ display_cell(C) ++ " |"),
    output:display_message("-------------"),
    output:display_message("| " ++ display_cell(D) ++ " | " ++ display_cell(E) ++ " | " ++ display_cell(F) ++ " |"),
    output:display_message("-------------"),
    output:display_message("| " ++ display_cell(G) ++ " | " ++ display_cell(H) ++ " | " ++ display_cell(I) ++ " |"),
    output:display_message("-------------").

display_cell(Cell) ->
    case Cell of
        "X" -> "X";
        "O" -> "O";
        _ -> integer_to_list(Cell)
    end.