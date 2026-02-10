-module(lexer_error).
-export([new/3, new/4, format_message/1]).

-record(lexer_error, {
    message   :: string(),
    line      :: pos_integer(),
    column    :: pos_integer(),
    character :: string() | undefined
}).

%% Create a new lexer error without a character
-spec new(string(), pos_integer(), pos_integer()) -> #lexer_error{}.
new(Message, Line, Column) ->
    #lexer_error{message = Message, line = Line, column = Column, character = undefined}.

%% Create a new lexer error with the offending character
-spec new(string(), pos_integer(), pos_integer(), string()) -> #lexer_error{}.
new(Message, Line, Column, Character) ->
    #lexer_error{message = Message, line = Line, column = Column, character = Character}.

%% Format the error into a human-readable string
-spec format_message(#lexer_error{}) -> string().
format_message(#lexer_error{message = Msg, line = Line, column = Col, character = undefined}) ->
    lists:flatten(io_lib:format("Lexer Error: ~s at line ~p, column ~p", [Msg, Line, Col]));
format_message(#lexer_error{message = Msg, line = Line, column = Col, character = Char}) ->
    lists:flatten(io_lib:format("Lexer Error: ~s '~s' at line ~p, column ~p", [Msg, Char, Line, Col])).
