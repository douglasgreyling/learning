-module(token).
-export([new/4, to_string/1, is_type/2, is_one_of/2, keyword_type/1]).

-include("token.hrl").

%% Create a new token record
-spec new(atom(), string(), pos_integer(), pos_integer()) -> #token{}.
new(Type, Value, Line, Column) ->
    #token{type = Type, value = Value, line = Line, column = Column}.

%% Convert a token to a human-readable string
-spec to_string(#token{}) -> string().
to_string(#token{type = Type, value = Value, line = Line, column = Column}) ->
    io_lib:format("Token(~p, ~p, ~p:~p)", [Type, Value, Line, Column]).

%% Check if a token matches a given type
-spec is_type(#token{}, atom()) -> boolean().
is_type(#token{type = Type}, ExpectedType) ->
    Type =:= ExpectedType.

%% Check if a token type is one of a list of types
-spec is_one_of(#token{}, [atom()]) -> boolean().
is_one_of(#token{type = Type}, Types) ->
    lists:member(Type, Types).

%% Look up whether an identifier is a keyword, returning its token type.
%% Returns 'identifier' if not a keyword.
-spec keyword_type(string()) -> atom().
keyword_type("if")       -> 'if';
keyword_type("else")     -> 'else';
keyword_type("while")    -> 'while';
keyword_type("for")      -> 'for';
keyword_type("function") -> 'function';
keyword_type("return")   -> 'return';
keyword_type("let")      -> 'let';
keyword_type("const")    -> 'const';
keyword_type("true")     -> 'true';
keyword_type("false")    -> 'false';
keyword_type("null")     -> 'null';
keyword_type(_)          -> identifier.
