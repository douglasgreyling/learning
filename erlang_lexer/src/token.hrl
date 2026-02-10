%% Token record definition
%% Type: atom representing the token type
%% Value: string containing the token's text
%% Line: line number where the token was found
%% Column: column number where the token starts
-record(token, {
    type   :: atom(),
    value  :: string(),
    line   :: pos_integer(),
    column :: pos_integer()
}).

%% Token type atoms:
%%
%% Literals:      number, string, identifier
%% Keywords:      'if', 'else', 'while', 'for', 'function', 'return',
%%                'let', 'const', 'true', 'false', 'null'
%% Operators:     plus, minus, multiply, divide, modulo,
%%                assign, equals, not_equals,
%%                less_than, greater_than, less_equal, greater_equal,
%%                logical_and, logical_or, logical_not
%% Delimiters:    lparen, rparen, lbrace, rbrace,
%%                lbracket, rbracket, semicolon, comma, dot
%% Special:       eof, newline
