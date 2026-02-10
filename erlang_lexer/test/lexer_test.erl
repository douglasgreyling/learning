-module(lexer_test).
-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").

%% Helper: tokenize and return the token list (asserting success).
tokenize(Source) ->
    {ok, Tokens} = lexer:tokenize(Source),
    Tokens.

%% Helper: return tokens excluding the trailing EOF.
tokens_no_eof(Source) ->
    Tokens = tokenize(Source),
    lists:filter(fun(#token{type = T}) -> T =/= eof end, Tokens).

%% ===================================================================
%% Basic Tokenization
%% ===================================================================

number_integer_test() ->
    [#token{type = number, value = "42"} | _] = tokenize("42").

number_float_test() ->
    [#token{type = number, value = "3.14"} | _] = tokenize("3.14").

string_double_quote_test() ->
    [#token{type = string, value = "hello"} | _] = tokenize("\"hello\"").

string_single_quote_test() ->
    [#token{type = string, value = "world"} | _] = tokenize("'world'").

identifier_test() ->
    [#token{type = identifier, value = "myVar"} | _] = tokenize("myVar").

identifier_with_underscore_test() ->
    [#token{type = identifier, value = "_private"} | _] = tokenize("_private").

%% ===================================================================
%% Keywords
%% ===================================================================

keyword_if_test() ->
    [#token{type = 'if', value = "if"} | _] = tokenize("if").

keyword_else_test() ->
    [#token{type = 'else', value = "else"} | _] = tokenize("else").

keyword_while_test() ->
    [#token{type = 'while', value = "while"} | _] = tokenize("while").

keyword_for_test() ->
    [#token{type = 'for', value = "for"} | _] = tokenize("for").

keyword_function_test() ->
    [#token{type = 'function', value = "function"} | _] = tokenize("function").

keyword_return_test() ->
    [#token{type = 'return', value = "return"} | _] = tokenize("return").

keyword_let_test() ->
    [#token{type = 'let', value = "let"} | _] = tokenize("let").

keyword_const_test() ->
    [#token{type = 'const', value = "const"} | _] = tokenize("const").

keyword_true_test() ->
    [#token{type = 'true', value = "true"} | _] = tokenize("true").

keyword_false_test() ->
    [#token{type = 'false', value = "false"} | _] = tokenize("false").

keyword_null_test() ->
    [#token{type = 'null', value = "null"} | _] = tokenize("null").

%% ===================================================================
%% Operators
%% ===================================================================

operator_plus_test() ->
    [#token{type = plus, value = "+"} | _] = tokenize("+").

operator_minus_test() ->
    [#token{type = minus, value = "-"} | _] = tokenize("-").

operator_multiply_test() ->
    [#token{type = multiply, value = "*"} | _] = tokenize("*").

operator_divide_test() ->
    [#token{type = divide, value = "/"} | _] = tokenize("/").

operator_modulo_test() ->
    [#token{type = modulo, value = "%"} | _] = tokenize("%").

operator_assign_test() ->
    [#token{type = assign, value = "="} | _] = tokenize("=").

operator_equals_test() ->
    [#token{type = equals, value = "=="} | _] = tokenize("==").

operator_not_equals_test() ->
    [#token{type = not_equals, value = "!="} | _] = tokenize("!=").

operator_less_than_test() ->
    [#token{type = less_than, value = "<"} | _] = tokenize("<").

operator_greater_than_test() ->
    [#token{type = greater_than, value = ">"} | _] = tokenize(">").

operator_less_equal_test() ->
    [#token{type = less_equal, value = "<="} | _] = tokenize("<=").

operator_greater_equal_test() ->
    [#token{type = greater_equal, value = ">="} | _] = tokenize(">=").

operator_logical_and_test() ->
    [#token{type = logical_and, value = "&&"} | _] = tokenize("&&").

operator_logical_or_test() ->
    [#token{type = logical_or, value = "||"} | _] = tokenize("||").

operator_logical_not_test() ->
    [#token{type = logical_not, value = "!"} | _] = tokenize("!").

%% ===================================================================
%% Delimiters
%% ===================================================================

delimiter_lparen_test() ->
    [#token{type = lparen} | _] = tokenize("(").

delimiter_rparen_test() ->
    [#token{type = rparen} | _] = tokenize(")").

delimiter_lbrace_test() ->
    [#token{type = lbrace} | _] = tokenize("{").

delimiter_rbrace_test() ->
    [#token{type = rbrace} | _] = tokenize("}").

delimiter_lbracket_test() ->
    [#token{type = lbracket} | _] = tokenize("[").

delimiter_rbracket_test() ->
    [#token{type = rbracket} | _] = tokenize("]").

delimiter_semicolon_test() ->
    [#token{type = semicolon} | _] = tokenize(";").

delimiter_comma_test() ->
    [#token{type = comma} | _] = tokenize(",").

delimiter_dot_test() ->
    [#token{type = dot} | _] = tokenize(".").

%% ===================================================================
%% Comments
%% ===================================================================

line_comment_test() ->
    Tokens = tokens_no_eof("42 // this is a comment"),
    ?assertEqual(1, length(Tokens)),
    [#token{type = number, value = "42"}] = Tokens.

block_comment_test() ->
    Tokens = tokens_no_eof("42 /* block comment */ 7"),
    ?assertEqual(2, length(Tokens)),
    [#token{type = number, value = "42"}, #token{type = number, value = "7"}] = Tokens.

nested_block_comment_test() ->
    Tokens = tokens_no_eof("1 /* outer /* inner */ still comment */ 2"),
    ?assertEqual(2, length(Tokens)),
    [#token{type = number, value = "1"}, #token{type = number, value = "2"}] = Tokens.

%% ===================================================================
%% Complex Expressions
%% ===================================================================

simple_expression_test() ->
    Tokens = tokens_no_eof("let x = 42 + 3.14;"),
    Types = [T#token.type || T <- Tokens],
    ?assertEqual(['let', identifier, assign, number, plus, number, semicolon], Types).

function_declaration_test() ->
    Tokens = tokens_no_eof("function add(a, b) { return a + b; }"),
    Types = [T#token.type || T <- Tokens],
    ?assertEqual(['function', identifier, lparen, identifier, comma, identifier, rparen,
                  lbrace, 'return', identifier, plus, identifier, semicolon, rbrace], Types).

conditional_test() ->
    Tokens = tokens_no_eof("if (x > 10) { return true; }"),
    Types = [T#token.type || T <- Tokens],
    ?assertEqual(['if', lparen, identifier, greater_than, number, rparen,
                  lbrace, 'return', 'true', semicolon, rbrace], Types).

%% ===================================================================
%% Position Tracking
%% ===================================================================

position_tracking_test() ->
    [T1, T2, T3 | _] = tokenize("let x"),
    ?assertEqual({1, 1}, {T1#token.line, T1#token.column}),
    ?assertEqual({1, 5}, {T2#token.line, T2#token.column}),
    %% EOF
    ?assertEqual({1, 6}, {T3#token.line, T3#token.column}).

multiline_position_test() ->
    Tokens = tokens_no_eof("let\nx"),
    [T1, T2] = Tokens,
    ?assertEqual({1, 1}, {T1#token.line, T1#token.column}),
    ?assertEqual({2, 1}, {T2#token.line, T2#token.column}).

%% ===================================================================
%% Error Cases
%% ===================================================================

unterminated_string_test() ->
    ?assertMatch({error, _}, lexer:tokenize("\"hello")).

invalid_single_ampersand_test() ->
    ?assertMatch({error, _}, lexer:tokenize("&")).

invalid_single_pipe_test() ->
    ?assertMatch({error, _}, lexer:tokenize("|")).

unexpected_character_test() ->
    ?assertMatch({error, _}, lexer:tokenize("@")).

unterminated_block_comment_test() ->
    ?assertMatch({error, _}, lexer:tokenize("/* unterminated")).

%% ===================================================================
%% String Escape Sequences
%% ===================================================================

string_escape_newline_test() ->
    [#token{type = string, value = Value} | _] = tokenize("\"hello\\nworld\""),
    ?assertEqual("hello\nworld", Value).

string_escape_tab_test() ->
    [#token{type = string, value = Value} | _] = tokenize("\"hello\\tworld\""),
    ?assertEqual("hello\tworld", Value).

string_escape_backslash_test() ->
    [#token{type = string, value = Value} | _] = tokenize("\"hello\\\\world\""),
    ?assertEqual("hello\\world", Value).

%% ===================================================================
%% Token Module Functions
%% ===================================================================

token_to_string_test() ->
    T = token:new(number, "42", 1, 1),
    Str = lists:flatten(token:to_string(T)),
    ?assert(is_list(Str)),
    ?assertNotEqual(0, length(Str)).

token_is_type_test() ->
    T = token:new(number, "42", 1, 1),
    ?assert(token:is_type(T, number)),
    ?assertNot(token:is_type(T, string)).

token_is_one_of_test() ->
    T = token:new(number, "42", 1, 1),
    ?assert(token:is_one_of(T, [number, string, identifier])),
    ?assertNot(token:is_one_of(T, [string, identifier])).

%% ===================================================================
%% EOF Token
%% ===================================================================

eof_token_test() ->
    Tokens = tokenize(""),
    ?assertEqual(1, length(Tokens)),
    [#token{type = eof}] = Tokens.

eof_at_end_test() ->
    Tokens = tokenize("42"),
    Last = lists:last(Tokens),
    ?assertEqual(eof, Last#token.type).

%% ===================================================================
%% Lexer Error Module
%% ===================================================================

lexer_error_format_test() ->
    Err = lexer_error:new("Test error", 5, 10),
    Msg = lexer_error:format_message(Err),
    ?assert(is_list(Msg)),
    ?assertNotEqual(nomatch, string:find(Msg, "Test error")),
    ?assertNotEqual(nomatch, string:find(Msg, "5")),
    ?assertNotEqual(nomatch, string:find(Msg, "10")).

lexer_error_format_with_char_test() ->
    Err = lexer_error:new("Bad char", 1, 1, "@"),
    Msg = lexer_error:format_message(Err),
    ?assertNotEqual(nomatch, string:find(Msg, "@")).
