-module(main).
-export([start/0]).

-include("token.hrl").

%% Entry point for the lexer demo.
start() ->
    io:format("~n========================================~n"),
    io:format("     Erlang Lexer Demonstration~n"),
    io:format("========================================~n~n"),

    %% Demo 1: Inline source code
    tokenize_source("let x = 42 + 3.14;", "Simple Expression"),
    tokenize_source("if (x > 10) { return true; } else { return false; }", "Conditional"),

    %% Demo 2: Read and tokenize example files
    tokenize_file("simple.txt", "File: simple.txt"),
    tokenize_file("conditional.txt", "File: conditional.txt"),
    tokenize_file("function.txt", "File: function.txt"),
    tokenize_file("loop.txt", "File: loop.txt"),

    %% Demo 3: Read multiple files concurrently
    io:format("--- Concurrent File Reading ---~n"),
    Paths = [file_reader:get_example_path(F) || F <- ["simple.txt", "conditional.txt", "function.txt", "loop.txt"]],
    Results = file_reader:read_multiple_files(Paths),
    lists:foreach(
        fun({Path, {ok, Contents}}) ->
            Name = filename:basename(Path),
            io:format("  Read ~s (~p bytes)~n", [Name, length(Contents)]);
           ({Path, {error, Reason}}) ->
            Name = filename:basename(Path),
            io:format("  Error reading ~s: ~p~n", [Name, Reason])
        end,
        Results
    ),

    io:format("~n========================================~n"),
    io:format("     Demonstration Complete~n"),
    io:format("========================================~n~n"),
    ok.

%% Tokenize an inline source string and display the tokens.
tokenize_source(Source, Label) ->
    io:format("--- ~s ---~n", [Label]),
    io:format("Source: ~s~n~n", [Source]),
    case lexer:tokenize(Source) of
        {ok, Tokens} ->
            display_tokens(Tokens),
            io:format("~n");
        {error, Error} ->
            io:format("Error: ~s~n~n", [lexer_error:format_message(Error)])
    end.

%% Read a file from the examples directory and tokenize it.
tokenize_file(Filename, Label) ->
    Path = file_reader:get_example_path(Filename),
    case file_reader:read_file(Path) of
        {ok, Contents} ->
            io:format("--- ~s ---~n", [Label]),
            io:format("Source: ~s~n~n", [string:trim(Contents)]),
            case lexer:tokenize(Contents) of
                {ok, Tokens} ->
                    display_tokens(Tokens),
                    io:format("~n");
                {error, Error} ->
                    io:format("Error: ~s~n~n", [lexer_error:format_message(Error)])
            end;
        {error, Reason} ->
            io:format("--- ~s ---~n", [Label]),
            io:format("Could not read file: ~p~n~n", [Reason])
    end.

%% Display a list of tokens in a formatted table.
display_tokens(Tokens) ->
    io:format("  ~-20s ~-20s ~s~n", ["TYPE", "VALUE", "POSITION"]),
    io:format("  ~s ~s ~s~n", [string:copies("-", 20), string:copies("-", 20), string:copies("-", 10)]),
    lists:foreach(
        fun(#token{type = Type, value = Value, line = Line, column = Col}) ->
            TypeStr = string:uppercase(atom_to_list(Type)),
            PosStr = io_lib:format("~p:~p", [Line, Col]),
            DisplayValue = case Value of
                "" -> "(empty)";
                _  -> Value
            end,
            io:format("  ~-20s ~-20s ~s~n", [TypeStr, DisplayValue, PosStr])
        end,
        Tokens
    ).
