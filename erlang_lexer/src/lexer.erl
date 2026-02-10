-module(lexer).
-export([tokenize/1]).

-include("token.hrl").

%% Main entry point: tokenize a source string into a list of tokens.
-spec tokenize(string()) -> {ok, [#token{}]} | {error, term()}.
tokenize(Source) ->
    try
        Tokens = scan_tokens(Source, 1, 1, []),
        {ok, Tokens}
    catch
        throw:{lexer_error, Error} -> {error, Error}
    end.

%% Recursively scan the source string, accumulating tokens.
%% Returns the token list in correct order with an EOF token appended.
-spec scan_tokens(string(), pos_integer(), pos_integer(), [#token{}]) -> [#token{}].
scan_tokens([], Line, Column, Acc) ->
    lists:reverse([token:new(eof, "", Line, Column) | Acc]);
scan_tokens(Source, Line, Column, Acc) ->
    case scan_token(Source, Line, Column) of
        {skip, Rest, NewLine, NewCol} ->
            scan_tokens(Rest, NewLine, NewCol, Acc);
        {token, Token, Rest, NewLine, NewCol} ->
            scan_tokens(Rest, NewLine, NewCol, [Token | Acc])
    end.

%% Scan a single token from the head of the source string.
%% Returns either {skip, ...} for whitespace/comments or {token, ...} for real tokens.

%% Whitespace
scan_token([$\s | Rest], Line, Col) ->
    {skip, Rest, Line, Col + 1};
scan_token([$\t | Rest], Line, Col) ->
    {skip, Rest, Line, Col + 1};
scan_token([$\r | Rest], Line, Col) ->
    {skip, Rest, Line, Col + 1};
scan_token([$\n | Rest], Line, _Col) ->
    {skip, Rest, Line + 1, 1};

%% Line comment: //
scan_token([$/,$/ | Rest], Line, Col) ->
    {Remaining, NewLine, NewCol} = skip_line_comment(Rest, Line, Col + 2),
    {skip, Remaining, NewLine, NewCol};

%% Block comment: /* */
scan_token([$/,$* | Rest], Line, Col) ->
    {Remaining, NewLine, NewCol} = skip_block_comment(Rest, Line, Col + 2, 1),
    {skip, Remaining, NewLine, NewCol};

%% Two-character operators
scan_token([$=,$= | Rest], Line, Col) ->
    {token, token:new(equals, "==", Line, Col), Rest, Line, Col + 2};
scan_token([$!,$= | Rest], Line, Col) ->
    {token, token:new(not_equals, "!=", Line, Col), Rest, Line, Col + 2};
scan_token([$<,$= | Rest], Line, Col) ->
    {token, token:new(less_equal, "<=", Line, Col), Rest, Line, Col + 2};
scan_token([$>,$= | Rest], Line, Col) ->
    {token, token:new(greater_equal, ">=", Line, Col), Rest, Line, Col + 2};
scan_token([$&,$& | Rest], Line, Col) ->
    {token, token:new(logical_and, "&&", Line, Col), Rest, Line, Col + 2};
scan_token([$|,$| | Rest], Line, Col) ->
    {token, token:new(logical_or, "||", Line, Col), Rest, Line, Col + 2};

%% Single-character operators and delimiters
scan_token([$+ | Rest], Line, Col) ->
    {token, token:new(plus, "+", Line, Col), Rest, Line, Col + 1};
scan_token([$- | Rest], Line, Col) ->
    {token, token:new(minus, "-", Line, Col), Rest, Line, Col + 1};
scan_token([$* | Rest], Line, Col) ->
    {token, token:new(multiply, "*", Line, Col), Rest, Line, Col + 1};
scan_token([$/ | Rest], Line, Col) ->
    {token, token:new(divide, "/", Line, Col), Rest, Line, Col + 1};
scan_token([$% | Rest], Line, Col) ->
    {token, token:new(modulo, "%", Line, Col), Rest, Line, Col + 1};
scan_token([$= | Rest], Line, Col) ->
    {token, token:new(assign, "=", Line, Col), Rest, Line, Col + 1};
scan_token([$< | Rest], Line, Col) ->
    {token, token:new(less_than, "<", Line, Col), Rest, Line, Col + 1};
scan_token([$> | Rest], Line, Col) ->
    {token, token:new(greater_than, ">", Line, Col), Rest, Line, Col + 1};
scan_token([$! | Rest], Line, Col) ->
    {token, token:new(logical_not, "!", Line, Col), Rest, Line, Col + 1};
scan_token([$( | Rest], Line, Col) ->
    {token, token:new(lparen, "(", Line, Col), Rest, Line, Col + 1};
scan_token([$) | Rest], Line, Col) ->
    {token, token:new(rparen, ")", Line, Col), Rest, Line, Col + 1};
scan_token([${ | Rest], Line, Col) ->
    {token, token:new(lbrace, "{", Line, Col), Rest, Line, Col + 1};
scan_token([$} | Rest], Line, Col) ->
    {token, token:new(rbrace, "}", Line, Col), Rest, Line, Col + 1};
scan_token([$[ | Rest], Line, Col) ->
    {token, token:new(lbracket, "[", Line, Col), Rest, Line, Col + 1};
scan_token([$] | Rest], Line, Col) ->
    {token, token:new(rbracket, "]", Line, Col), Rest, Line, Col + 1};
scan_token([$; | Rest], Line, Col) ->
    {token, token:new(semicolon, ";", Line, Col), Rest, Line, Col + 1};
scan_token([$, | Rest], Line, Col) ->
    {token, token:new(comma, ",", Line, Col), Rest, Line, Col + 1};
scan_token([$. | Rest], Line, Col) ->
    {token, token:new(dot, ".", Line, Col), Rest, Line, Col + 1};

%% String literals (double or single quote)
scan_token([$" | Rest], Line, Col) ->
    {Value, Remaining, NewLine, NewCol} = scan_string(Rest, $", Line, Col + 1, []),
    {token, token:new(string, Value, Line, Col), Remaining, NewLine, NewCol};
scan_token([$' | Rest], Line, Col) ->
    {Value, Remaining, NewLine, NewCol} = scan_string(Rest, $', Line, Col + 1, []),
    {token, token:new(string, Value, Line, Col), Remaining, NewLine, NewCol};

%% Invalid single & or |
scan_token([$& | _Rest], Line, Col) ->
    throw({lexer_error, lexer_error:new("Invalid character", Line, Col, "&")});
scan_token([$| | _Rest], Line, Col) ->
    throw({lexer_error, lexer_error:new("Invalid character", Line, Col, "|")});

%% Numbers and identifiers handled via guards
scan_token([C | Rest], Line, Col) when C >= $0, C =< $9 ->
    {Value, Remaining, NewCol} = scan_number(Rest, Col + 1, [C]),
    {token, token:new(number, Value, Line, Col), Remaining, Line, NewCol};
scan_token([C | Rest], Line, Col) when (C >= $a andalso C =< $z);
                                       (C >= $A andalso C =< $Z);
                                       C =:= $_ ->
    {Value, Remaining, NewCol} = scan_identifier(Rest, Col + 1, [C]),
    Type = token:keyword_type(Value),
    {token, token:new(Type, Value, Line, Col), Remaining, Line, NewCol};

%% Unknown character
scan_token([C | _Rest], Line, Col) ->
    throw({lexer_error, lexer_error:new("Unexpected character", Line, Col, [C])}).

%% --- String scanning ---
%% Scans characters inside a string literal until the closing quote.
%% Handles escape sequences like \n, \t, \\, \", \'.
scan_string([], _Quote, Line, Col, _Acc) ->
    throw({lexer_error, lexer_error:new("Unterminated string", Line, Col)});
scan_string([$\n | _Rest], _Quote, Line, Col, _Acc) ->
    throw({lexer_error, lexer_error:new("Unterminated string", Line, Col)});
scan_string([Quote | Rest], Quote, _Line, Col, Acc) ->
    {lists:reverse(Acc), Rest, _Line, Col + 1};
scan_string([$\\, $n | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$\n | Acc]);
scan_string([$\\, $t | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$\t | Acc]);
scan_string([$\\, $r | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$\r | Acc]);
scan_string([$\\, $\\ | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$\\ | Acc]);
scan_string([$\\, $" | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$" | Acc]);
scan_string([$\\, $' | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 2, [$' | Acc]);
scan_string([C | Rest], Quote, Line, Col, Acc) ->
    scan_string(Rest, Quote, Line, Col + 1, [C | Acc]).

%% --- Number scanning ---
%% Recursively consumes digits, then checks for a decimal point followed by more digits.
scan_number([C | Rest], Col, Acc) when C >= $0, C =< $9 ->
    scan_number(Rest, Col + 1, [C | Acc]);
scan_number([$., C | Rest], Col, Acc) when C >= $0, C =< $9 ->
    %% Decimal point followed by a digit: consume fractional part
    scan_number_fraction(Rest, Col + 2, [C, $. | Acc]);
scan_number(Rest, Col, Acc) ->
    {lists:reverse(Acc), Rest, Col}.

%% Recursively consume digits after the decimal point.
scan_number_fraction([C | Rest], Col, Acc) when C >= $0, C =< $9 ->
    scan_number_fraction(Rest, Col + 1, [C | Acc]);
scan_number_fraction(Rest, Col, Acc) ->
    {lists:reverse(Acc), Rest, Col}.

%% --- Identifier scanning ---
%% Recursively consumes alphanumeric characters and underscores.
scan_identifier([C | Rest], Col, Acc) when (C >= $a andalso C =< $z);
                                           (C >= $A andalso C =< $Z);
                                           (C >= $0 andalso C =< $9);
                                           C =:= $_ ->
    scan_identifier(Rest, Col + 1, [C | Acc]);
scan_identifier(Rest, Col, Acc) ->
    {lists:reverse(Acc), Rest, Col}.

%% --- Comment handling ---
%% Skip characters until end of line for // comments.
skip_line_comment([$\n | Rest], Line, _Col) ->
    {Rest, Line + 1, 1};
skip_line_comment([], Line, Col) ->
    {[], Line, Col};
skip_line_comment([_ | Rest], Line, Col) ->
    skip_line_comment(Rest, Line, Col + 1).

%% Skip block comments with nesting support.
%% Depth tracks the nesting level; we return when depth reaches 0.
skip_block_comment([], Line, Col, _Depth) ->
    throw({lexer_error, lexer_error:new("Unterminated block comment", Line, Col)});
skip_block_comment([$*, $/ | Rest], Line, Col, 1) ->
    {Rest, Line, Col + 2};
skip_block_comment([$*, $/ | Rest], Line, Col, Depth) ->
    skip_block_comment(Rest, Line, Col + 2, Depth - 1);
skip_block_comment([$/, $* | Rest], Line, Col, Depth) ->
    skip_block_comment(Rest, Line, Col + 2, Depth + 1);
skip_block_comment([$\n | Rest], Line, _Col, Depth) ->
    skip_block_comment(Rest, Line + 1, 1, Depth);
skip_block_comment([_ | Rest], Line, Col, Depth) ->
    skip_block_comment(Rest, Line, Col + 1, Depth).
