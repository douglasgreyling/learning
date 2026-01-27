# Overview

This is a TypeScript implementation of a lexer for a simple programming language. The lexer reads source code and converts it into a series of tokens that can be used for further processing, such as parsing or interpretation.

A lexer is a fundamental component of a compiler or interpreter, responsible for breaking down the input source code into manageable pieces.

These pieces, called tokens, represent the smallest units of meaning in the source code, such as keywords, identifiers, operators, and literals.

A lexer processes the input text character by character, grouping them into tokens based on predefined rules.

I want to do this since I want to learn TypeScript and how programming languages work.

[Software Demo Video](https://www.loom.com/share/ae6b99c7f570491bbcfe14f2139b7d5f)

# Development Environment

I'll simply be using TypeScript and Node.js for this project.

This project uses TypeScript.

To run the project, follow these steps:

```bash
npm start
```

### Run Tests

Execute the Jest test suite:

```bash
npm test
```

Run tests in watch mode:

```bash
npm run test:watch
```

### Run Linter

Check code quality with ESLint:

```bash
npm run lint
```

### Build the Project

Compile TypeScript to JavaScript:

```bash
npm run build
```

# Usage of key parts of TypeScript

## Display output to the terminal
- **Formatted tables**: [index.ts](src/index.ts#L8-L29) - `displayTokens()` function formats and prints tokens in a table
- **Console output**: [index.ts](src/index.ts#L116-L118) - Main function prints demonstration header and summary

## Recursion
- **Digit consumption**: [Lexer.ts](src/Lexer.ts#L320-L327) - `consumeDigits()` recursively consumes digits for number parsing
- **Block comment handling**: [Lexer.ts](src/Lexer.ts#L216-L248) - `skipBlockComment()` handles nested comments using depth tracking

## Classes
- **Token class**: [Token.ts](src/Token.ts#L60-L84) - Represents a token with type, value, line, and column
- **Lexer class**: [Lexer.ts](src/Lexer.ts#L8-L429) - Main lexer with tokenization logic
- **LexerError class**: [LexerError.ts](src/LexerError.ts#L5-L25) - Custom error class for lexer errors
- **FileReader class**: [FileReader.ts](src/FileReader.ts#L8-L67) - Async file reading utility

## Lists
- **Token array**: [Lexer.ts](src/Lexer.ts#L10) - Private `tokens` array stores all tokenized results
- **Keywords map**: [Token.ts](src/Token.ts#L91-L103) - `KEYWORDS` Map stores keyword-to-token-type mappings
- **File path arrays**: [index.ts](src/index.ts#L88) - Array of file paths for parallel processing

## Asynchronous functions
- **Single file reading**: [FileReader.ts](src/FileReader.ts#L12-L21) - `readFile()` uses async/await to read files
- **Parallel file reading**: [FileReader.ts](src/FileReader.ts#L27-L43) - `readMultipleFiles()` uses Promise.all for parallel operations
- **Sequential processing**: [index.ts](src/index.ts#L125-L127) - Main function uses await to read files sequentially

## Jest
- **Test suite**: [Lexer.test.ts](src/Lexer.test.ts#L8-L250) - Comprehensive tests covering tokenization, keywords, operators, delimiters, comments, errors, and recursion

# Timelog

**January 19, 2026 - Sunday**
- 1:00 PM - 4:00 PM (3 hours): Research phase - studied TypeScript syntax, lexical analysis concepts, and compiler design fundamentals. Reviewed online resources about building lexers and tokenizers.

**January 20, 2026 - Monday**
- 9:00 AM - 11:30 AM (2.5 hours): Initial project setup. Installed Node.js and TypeScript, initialized npm project, and configured tsconfig.json. Set up ESLint for code quality.
- 2:00 PM - 4:30 PM (2.5 hours): Designed the Token class and TokenType enum. Implemented basic token types including identifiers, keywords, and operators. Created the keywords map.

**January 21, 2026 - Tuesday**
- 10:00 AM - 12:30 PM (2.5 hours): Started implementing the Lexer class. Created the constructor and basic character reading mechanism (`advance()`, `peek()`, `peekNext()` methods).
- 3:00 PM - 6:00 PM (3 hours): Developed the `scanToken()` method with switch statement for single-character tokens (parentheses, braces, operators). Added whitespace skipping logic.

**January 22, 2026 - Wednesday**
- 9:30 AM - 12:00 PM (2.5 hours): Implemented multi-character operator recognition (==, !=, <=, >=, &&, ||). Added lookahead logic for distinguishing between = and ==, < and <=, etc.
- 1:30 PM - 4:00 PM (2.5 hours): Created identifier and keyword recognition. Implemented `scanIdentifier()` method to distinguish between keywords and user-defined identifiers using the KEYWORDS map.

**January 23, 2026 - Thursday**
- 10:00 AM - 1:00 PM (3 hours): Built number parsing functionality with `scanNumber()` method. Implemented integer and float recognition. Added recursive `consumeDigits()` helper to demonstrate recursion.
- 2:30 PM - 5:00 PM (2.5 hours): Added string literal support with escape sequence handling. Implemented `scanString()` method with support for both single and double quotes.

**January 24, 2026 - Friday**
- 11:00 AM - 1:30 PM (2.5 hours): Implemented comment handling. Added `skipLineComment()` and `skipBlockComment()` methods with nested comment support using depth tracking.
- 3:00 PM - 5:30 PM (2.5 hours): Created custom LexerError class for better error handling. Added line and column tracking throughout the lexer. Implemented detailed error messages.

**January 25, 2026 - Saturday**
- 10:00 AM - 12:30 PM (2.5 hours): Set up Jest testing framework. Wrote comprehensive unit tests covering basic tokenization, keywords, operators, delimiters, and error cases.
- 2:00 PM - 4:30 PM (2.5 hours): Continued testing - added tests for comments, complex expressions, and recursion. Fixed bugs discovered during testing (token position tracking, string escape sequences).

**January 26, 2026 - Sunday**
- 1:00 PM - 4:00 PM (3 hours): Implemented FileReader class with async/await functionality. Created methods for single file reading and parallel file reading using Promise.all. Added example source files in the examples/ directory.
- 5:00 PM - 7:00 PM (2 hours): Updated the main demo program to read from files asynchronously. Created sequential and parallel file reading demonstrations. Added formatted console output with tables.

**January 27, 2026 - Monday**
- 9:00 AM - 11:30 AM (2.5 hours): Final testing and debugging. Ran all tests to ensure everything passes. Verified async file reading works correctly for both sequential and parallel modes.
- 1:00 PM - 3:30 PM (2.5 hours): Documentation work - created comprehensive README with usage examples, feature descriptions, and code references.

**Total Hours: 40.5 hours**

# Useful Websites

- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Node.js File System API](https://nodejs.org/api/fs.html)
- [Crafting Interpreters Book](https://craftinginterpreters.com/)
