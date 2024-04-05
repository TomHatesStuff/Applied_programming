% Stretch Challenge: Throw and Handle Exceptions
-module(exception_handling).
-export([handle_exception/1]).

handle_exception(X) when X < 0 -> 
    {error, "Invalid input: X cannot be negative"};
handle_exception(X) -> 
    X * 2.