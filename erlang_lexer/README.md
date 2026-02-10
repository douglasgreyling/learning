# Overview

This is an Erlang implementation of a lexer for a simple programming language. The lexer reads source code and converts it into a series of tokens that can be used for further processing, such as parsing or interpretation.

A lexer is a fundamental component of a compiler or interpreter, responsible for breaking down the input source code into manageable pieces.

These pieces, called tokens, represent the smallest units of meaning in the source code, such as keywords, identifiers, operators, and literals.

A lexer processes the input text character by character, grouping them into tokens based on predefined rules.

I want to do this since I want to learn Erlang and how programming languages work.

[Software Demo Video](https://www.loom.com/share/456570220e784b408426749a82ce7d3f)

# Development Environment

I'll simply be using Erlang and the Erlang runtime for this project.

This project uses Erlang. To install Erlang on macOS:

```bash
brew install erlang
```

To run the project, follow these steps:

```bash
make run
```

### Run Tests

Execute the EUnit test suite:

```bash
make test
```

### Build the Project

Compile Erlang source files to BEAM bytecode:

```bash
make compile
```

### Clean Build Artifacts

Remove compiled BEAM files:

```bash
make clean
```

# Usage of key parts of Erlang

## Display output to the terminal
- **Formatted tables**: [main.erl](src/main.erl#L83-L97) - `display_tokens/1` function formats and prints tokens in a table using `io:format/2`
- **Console output**: [main.erl](src/main.erl#L8-L13) - `start/0` function prints demonstration header and summary

## Recursion
- **Number scanning**: [lexer.erl](src/lexer.erl#L131-L138) - `scan_number/3` recursively consumes digits for number parsing
- **Identifier scanning**: [lexer.erl](src/lexer.erl#L145-L151) - `scan_identifier/3` recursively consumes alphanumeric characters
- **Block comment handling**: [lexer.erl](src/lexer.erl#L160-L170) - `skip_block_comment/4` handles nested comments with depth tracking
- **Token scanning**: [lexer.erl](src/lexer.erl#L20-L29) - `scan_tokens/4` recursively processes the entire source string

## Records
- **Token record**: [token.hrl](src/token.hrl#L6-L11) - Defines the token record with type, value, line, and column fields
- **Lexer error record**: [lexer_error.erl](src/lexer_error.erl#L4-L9) - Stores error details including message, position, and offending character

## Pattern Matching
- **Token scanning**: [lexer.erl](src/lexer.erl#L34-L124) - `scan_token/3` uses pattern matching on character lists for all token types
- **Keyword lookup**: [token.erl](src/token.erl#L31-L42) - `keyword_type/1` uses function clause pattern matching instead of a map
- **String escape sequences**: [lexer.erl](src/lexer.erl#L112-L127) - `scan_string/5` matches escape sequences like `\\n`, `\\t`, `\\\\`

## Lists
- **Token accumulation**: [lexer.erl](src/lexer.erl#L20-L29) - Tokens are accumulated in a reversed list and reversed at the end
- **List comprehensions**: [main.erl](src/main.erl#L27) - Used for building file paths from example filenames
- **File listing**: [file_reader.erl](src/file_reader.erl#L49-L52) - List comprehension filters `.txt` files

## Concurrent processes
- **Parallel file reading**: [file_reader.erl](src/file_reader.erl#L17-L30) - `read_multiple_files/1` spawns processes to read files concurrently
- **Message passing**: [file_reader.erl](src/file_reader.erl#L33-L40) - `collect_results/2` uses `receive` to gather results from spawned processes

## EUnit
- **Test suite**: [lexer_test.erl](test/lexer_test.erl) - 64 tests covering tokenization, keywords, operators, delimiters, comments, errors, escape sequences, and module functions

# Timelog

# Useful Websites

- [Erlang Documentation](https://www.erlang.org/doc/)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
- [Crafting Interpreters Book](https://craftinginterpreters.com/)
