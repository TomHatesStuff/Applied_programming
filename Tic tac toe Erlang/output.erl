% Display Output Module
-module(output).
-export([display_message/1]).

display_message(Message) ->
    io:format("~s~n", [Message]).

% create beam erlc tic_tac_toe.erl
% then enter erl
% tic_tac_toe:start().