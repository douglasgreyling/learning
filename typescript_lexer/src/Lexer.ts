import { Token, TokenType, KEYWORDS } from './Token';
import { LexerError } from './LexerError';

/**
 * Lexer class for tokenizing source code
 * Demonstrates: Classes, Recursion, Lists, and Exception handling
 */
export class Lexer {
  private source: string;
  private tokens: Token[];
  private current: number;
  private line: number;
  private column: number;

  constructor(source: string) {
    this.source = source;
    this.tokens = [];
    this.current = 0;
    this.line = 1;
    this.column = 1;
  }

  /**
   * Tokenizes the source code into tokens
   */
  tokenize(): Token[] {
    try {
      while (!this.isAtEnd()) {
        this.scanToken();
      }

      this.tokens.push(new Token(TokenType.EOF, '', this.line, this.column));
      return this.tokens;
    } catch (error) {
      if (error instanceof LexerError) {
        throw error;
      }
      throw new LexerError(
        `Unexpected error: ${error}`,
        this.line,
        this.column
      );
    }
  }

  /**
   * Scans a single token from the source
   */
  private scanToken(): void {
    this.skipWhitespace();

    if (this.isAtEnd()) {
      return;
    }

    const char = this.advance();

    // Single character tokens
    switch (char) {
      case '(':
        this.addToken(TokenType.LPAREN, char);
        break;
      case ')':
        this.addToken(TokenType.RPAREN, char);
        break;
      case '{':
        this.addToken(TokenType.LBRACE, char);
        break;
      case '}':
        this.addToken(TokenType.RBRACE, char);
        break;
      case '[':
        this.addToken(TokenType.LBRACKET, char);
        break;
      case ']':
        this.addToken(TokenType.RBRACKET, char);
        break;
      case ',':
        this.addToken(TokenType.COMMA, char);
        break;
      case '.':
        this.addToken(TokenType.DOT, char);
        break;
      case ';':
        this.addToken(TokenType.SEMICOLON, char);
        break;
      case '+':
        this.addToken(TokenType.PLUS, char);
        break;
      case '-':
        this.addToken(TokenType.MINUS, char);
        break;
      case '*':
        this.addToken(TokenType.MULTIPLY, char);
        break;
      case '%':
        this.addToken(TokenType.MODULO, char);
        break;
      case '/':
        // Handle comments or division
        if (this.peek() === '/') {
          this.skipLineComment();
        } else if (this.peek() === '*') {
          this.skipBlockComment(); // Demonstrates recursion
        } else {
          this.addToken(TokenType.DIVIDE, char);
        }
        break;
      case '=':
        if (this.peek() === '=') {
          this.advance();
          this.addToken(TokenType.EQUALS, '==');
        } else {
          this.addToken(TokenType.ASSIGN, char);
        }
        break;
      case '!':
        if (this.peek() === '=') {
          this.advance();
          this.addToken(TokenType.NOT_EQUALS, '!=');
        } else {
          this.addToken(TokenType.NOT, char);
        }
        break;
      case '<':
        if (this.peek() === '=') {
          this.advance();
          this.addToken(TokenType.LESS_EQUAL, '<=');
        } else {
          this.addToken(TokenType.LESS_THAN, char);
        }
        break;
      case '>':
        if (this.peek() === '=') {
          this.advance();
          this.addToken(TokenType.GREATER_EQUAL, '>=');
        } else {
          this.addToken(TokenType.GREATER_THAN, char);
        }
        break;
      case '&':
        if (this.peek() === '&') {
          this.advance();
          this.addToken(TokenType.AND, '&&');
        } else {
          throw new LexerError(
            'Unexpected character, expected &&',
            this.line,
            this.column - 1,
            char
          );
        }
        break;
      case '|':
        if (this.peek() === '|') {
          this.advance();
          this.addToken(TokenType.OR, '||');
        } else {
          throw new LexerError(
            'Unexpected character, expected ||',
            this.line,
            this.column - 1,
            char
          );
        }
        break;
      case '"':
      case "'":
        this.scanString(char);
        break;
      default:
        if (this.isDigit(char)) {
          this.scanNumber(char);
        } else if (this.isAlpha(char)) {
          this.scanIdentifier(char);
        } else {
          throw new LexerError(
            'Unexpected character',
            this.line,
            this.column - 1,
            char
          );
        }
    }
  }

  /**
   * Recursively skips block comments
   * Demonstrates recursion - handles nested comments
   */
  private skipBlockComment(): void {
    this.advance(); // consume '*'

    let depth = 1;

    while (!this.isAtEnd() && depth > 0) {
      const char = this.peek();

      if (char === '/' && this.peekNext() === '*') {
        // Nested comment - recursion happens through depth tracking
        depth++;
        this.advance();
        this.advance();
      } else if (char === '*' && this.peekNext() === '/') {
        depth--;
        this.advance();
        this.advance();
      } else {
        this.advance();
      }
    }

    if (depth > 0) {
      throw new LexerError(
        'Unterminated block comment',
        this.line,
        this.column
      );
    }
  }

  /**
   * Skips line comments
   */
  private skipLineComment(): void {
    while (!this.isAtEnd() && this.peek() !== '\n') {
      this.advance();
    }
  }

  /**
   * Scans a string literal
   */
  private scanString(quote: string): void {
    const startLine = this.line;
    const startColumn = this.column - 1;
    let value = '';

    while (!this.isAtEnd() && this.peek() !== quote) {
      if (this.peek() === '\n') {
        throw new LexerError(
          'Unterminated string literal',
          startLine,
          startColumn
        );
      }

      if (this.peek() === '\\') {
        this.advance();
        if (!this.isAtEnd()) {
          const escaped = this.advance();
          // Handle escape sequences
          switch (escaped) {
            case 'n':
              value += '\n';
              break;
            case 't':
              value += '\t';
              break;
            case 'r':
              value += '\r';
              break;
            case '\\':
              value += '\\';
              break;
            case quote:
              value += quote;
              break;
            default:
              value += escaped;
          }
        }
      } else {
        value += this.advance();
      }
    }

    if (this.isAtEnd()) {
      throw new LexerError('Unterminated string literal', startLine, startColumn);
    }

    this.advance(); // Consume closing quote
    this.addToken(TokenType.STRING, value);
  }

  /**
   * Scans a number literal
   * Demonstrates recursion through recursive number parsing
   */
  private scanNumber(firstDigit: string): void {
    let value = firstDigit;

    // Recursively consume digits
    value += this.consumeDigits();

    // Handle decimal numbers
    if (this.peek() === '.' && this.isDigit(this.peekNext())) {
      value += this.advance(); // consume '.'
      value += this.consumeDigits(); // recursively consume decimal digits
    }

    this.addToken(TokenType.NUMBER, value);
  }

  /**
   * Recursively consumes digits
   * Demonstrates recursion in a helper method
   */
  private consumeDigits(): string {
    if (this.isAtEnd() || !this.isDigit(this.peek())) {
      return '';
    }

    // Recursive case: consume current digit and recursively consume remaining
    return this.advance() + this.consumeDigits();
  }

  /**
   * Scans an identifier or keyword
   */
  private scanIdentifier(firstChar: string): void {
    let value = firstChar;

    while (!this.isAtEnd() && this.isAlphaNumeric(this.peek())) {
      value += this.advance();
    }

    // Check if it's a keyword
    const tokenType = KEYWORDS.get(value) || TokenType.IDENTIFIER;
    this.addToken(tokenType, value);
  }

  /**
   * Skips whitespace characters
   */
  private skipWhitespace(): void {
    while (!this.isAtEnd()) {
      const char = this.peek();
      if (char === ' ' || char === '\t' || char === '\r' || char === '\n') {
        this.advance();
      } else {
        break;
      }
    }
  }

  /**
   * Advances to the next character
   */
  private advance(): string {
    const char = this.source[this.current];
    this.current++;

    if (char === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }

    return char;
  }

  /**
   * Peeks at the current character without consuming it
   */
  private peek(): string {
    if (this.isAtEnd()) {
      return '\0';
    }
    return this.source[this.current];
  }

  /**
   * Peeks at the next character without consuming it
   */
  private peekNext(): string {
    if (this.current + 1 >= this.source.length) {
      return '\0';
    }
    return this.source[this.current + 1];
  }

  /**
   * Checks if we've reached the end of the source
   */
  private isAtEnd(): boolean {
    return this.current >= this.source.length;
  }

  /**
   * Adds a token to the tokens list
   */
  private addToken(type: TokenType, value: string): void {
    this.tokens.push(new Token(type, value, this.line, this.column - value.length));
  }

  /**
   * Checks if a character is a digit
   */
  private isDigit(char: string): boolean {
    return char >= '0' && char <= '9';
  }

  /**
   * Checks if a character is alphabetic or underscore
   */
  private isAlpha(char: string): boolean {
    return (
      (char >= 'a' && char <= 'z') ||
      (char >= 'A' && char <= 'Z') ||
      char === '_'
    );
  }

  /**
   * Checks if a character is alphanumeric or underscore
   */
  private isAlphaNumeric(char: string): boolean {
    return this.isAlpha(char) || this.isDigit(char);
  }

  /**
   * Gets the current list of tokens
   */
  getTokens(): Token[] {
    return this.tokens;
  }
}
