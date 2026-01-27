# Overview

This is a rust implementation of a lexer for a simple programming language. The lexer reads source code and converts it into a series of tokens that can be used for further processing, such as parsing or interpretation.

A lexer is a fundamental component of a compiler or interpreter, responsible for breaking down the input source code into manageable pieces.

These pieces, called tokens, represent the smallest units of meaning in the source code, such as keywords, identifiers, operators, and literals.

A lexer processes the input text character by character, grouping them into tokens based on predefined rules.

I want to do this since I want to learn rust and how programming languages work.

[Software Demo Video](https://www.loom.com/share/8d37f749525d4529893ec209271cfc68)

# Development Environment

I'll simply be using Rust and Cargo for this project.

This project uses Rust.

# Usage of key parts of rust

## Variables (mutable and immutable)
- **Immutable**: [lexer.rs](src/lexer/lexer.rs#L2-L3) - Constants `WHITESPACE_CHARS` and `SUFFIX_CHARS`
- **Mutable**: [lexer.rs](src/lexer/lexer.rs#L16-L19) - `lexer` variable in `new()` function

## Expressions
- **Match expression**: [lexer.rs](src/lexer/lexer.rs#L29-L165) - Complex pattern matching in `next_token()`
- **If-let expression**: [lexer.rs](src/lexer/lexer.rs#L215-L221) - Checking for character in `read_unicode_char()`

## Conditionals
- **If-else chains**: [lexer.rs](src/lexer/lexer.rs#L144-L148) - Checking identifier starting with underscore
- **Match arms with guards**: [lexer.rs](src/lexer/lexer.rs#L73-L79) - Peek character for multi-char operators

## Loops
- **While loop**: [lexer.rs](src/lexer/lexer.rs#L184-L186) - Skipping whitespace in `skip_whitespace()`
- **Loop with break**: [lexer.rs](src/lexer/lexer.rs#L273-L285) - Reading strings in `read_string()`

## Functions (ownership or reference)
- **Borrowing (&)**: [lexer.rs](src/lexer/lexer.rs#L14) - `new()` takes `&'a str` (immutable reference)
- **Mutable reference (&mut)**: [lexer.rs](src/lexer/lexer.rs#L25) - `next_token(&mut self)` modifies lexer state
- **Return with lifetime**: [lexer.rs](src/lexer/lexer.rs#L265) - Returns `&'a str` slice of input

## Object Oriented Techniques (struct and impl)
- **Struct definition**: [lexer.rs](src/lexer/lexer.rs#L6-L11) - `Lexer` struct with lifetime parameter
- **Impl block**: [lexer.rs](src/lexer/lexer.rs#L13-L420) - Methods implemented for `Lexer<'a>`
- **Trait implementation**: [lexer.rs](src/lexer/lexer.rs#L422-L433) - `Iterator` trait for `Lexer<'a>`

## Slicing
- **String slicing**: [lexer.rs](src/lexer/lexer.rs#L265) - `read_operator()` returns `&self.input[start..self.position + 1]`
- **Range slicing**: [lexer.rs](src/lexer/lexer.rs#L210) - `&self.input[self.read_position..]` for remaining input

# Timelog

**January 5, 2026 - Monday**
- 2:00 PM - 5:00 PM (3 hours): Research phase - studied lexical analysis concepts, compiler design fundamentals, and reviewed Rust ownership/lifetime documentation. Explored various lexer implementation strategies.

**January 6, 2026 - Tuesday**
- 9:00 AM - 11:30 AM (2.5 hours): Initial project setup. Installed Rust toolchain, created new Cargo project, and configured workspace. Studied lexer fundamentals from "Crafting Interpreters" book.
- 2:00 PM - 4:00 PM (2 hours): Designed the Token structure and TokenType enum. Implemented basic token types including identifiers, keywords, and operators.

**January 7, 2026 - Wednesday**
- 10:00 AM - 12:30 PM (2.5 hours): Implemented the Lexer struct with lifetime parameters. Created the basic character reading mechanism (`read_ascii_char` and `peek_ascii_char` methods).
- 3:00 PM - 5:30 PM (2.5 hours): Developed the `next_token()` method with initial pattern matching for single-character tokens (parentheses, braces, operators).

**January 9, 2026 - Friday**
- 1:00 PM - 4:00 PM (3 hours): Implemented multi-character operator recognition (==, !=, <=, >=, &&, ||, **). Added lookahead logic using `peek_ascii_char()`.
- 7:00 PM - 9:00 PM (2 hours): Created identifier and keyword recognition. Implemented `lookup_identifier()` function to distinguish between keywords and user-defined identifiers.

**January 10, 2026 - Saturday**
- 9:30 AM - 12:00 PM (2.5 hours): Built number parsing functionality including integer and float recognition with underscore separators. Implemented validation for malformed numbers.
- 2:30 PM - 4:30 PM (2 hours): Added string literal support with escape sequence handling. Implemented detection of unterminated strings.

**January 13, 2026 - Tuesday**
- 10:00 AM - 1:00 PM (3 hours): Implemented symbol recognition (`:symbol`) and symbol key support (`key:`). Added Unicode character support with `read_unicode_char()` and `peek_unicode_char()`.
- 4:00 PM - 6:00 PM (2 hours): Created comment handling. Implemented whitespace skipping logic.

**January 14, 2026 - Wednesday**
- 9:00 AM - 11:00 AM (2 hours): Implemented Iterator trait for the Lexer. This allows using the lexer in for loops and with iterator adapters.
- 1:30 PM - 4:30 PM (3 hours): Wrote comprehensive unit tests covering all token types, operators, edge cases, and error conditions. Debugged issues found during testing.

**January 15, 2026 - Thursday**
- 10:30 AM - 1:30 PM (3 hours): Fixed bugs in number parsing (consecutive underscores, trailing underscores). Refined identifier validation for edge cases like identifiers starting with underscore followed by digits.
- 3:00 PM - 5:00 PM (2 hours): Added support for identifier suffixes (`!` and `?`). Tested with Ruby-style method names.

**January 16, 2026 - Friday**
- 11:00 AM - 1:00 PM (2 hours): Code refactoring and cleanup. Improved error handling and added more descriptive comments throughout the codebase.

**January 17, 2026 - Saturday**
- 9:00 AM - 11:00 AM (2 hours): Documentation work - created README with project overview, updated Cargo.toml, and documented Rust feature usage with specific code examples.

**Total Hours: 41 hours**

# Useful Websites

- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
