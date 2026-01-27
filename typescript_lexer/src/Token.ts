/**
 * Enumeration of all token types in our scripting language
 */
export enum TokenType {
  // Literals
  NUMBER = 'NUMBER',
  STRING = 'STRING',
  IDENTIFIER = 'IDENTIFIER',

  // Keywords
  IF = 'IF',
  ELSE = 'ELSE',
  WHILE = 'WHILE',
  FOR = 'FOR',
  FUNCTION = 'FUNCTION',
  RETURN = 'RETURN',
  LET = 'LET',
  CONST = 'CONST',
  TRUE = 'TRUE',
  FALSE = 'FALSE',
  NULL = 'NULL',

  // Operators
  PLUS = 'PLUS',
  MINUS = 'MINUS',
  MULTIPLY = 'MULTIPLY',
  DIVIDE = 'DIVIDE',
  MODULO = 'MODULO',
  ASSIGN = 'ASSIGN',
  EQUALS = 'EQUALS',
  NOT_EQUALS = 'NOT_EQUALS',
  LESS_THAN = 'LESS_THAN',
  GREATER_THAN = 'GREATER_THAN',
  LESS_EQUAL = 'LESS_EQUAL',
  GREATER_EQUAL = 'GREATER_EQUAL',
  AND = 'AND',
  OR = 'OR',
  NOT = 'NOT',

  // Delimiters
  LPAREN = 'LPAREN',
  RPAREN = 'RPAREN',
  LBRACE = 'LBRACE',
  RBRACE = 'RBRACE',
  LBRACKET = 'LBRACKET',
  RBRACKET = 'RBRACKET',
  SEMICOLON = 'SEMICOLON',
  COMMA = 'COMMA',
  DOT = 'DOT',

  // Special
  EOF = 'EOF',
  NEWLINE = 'NEWLINE',
}

/**
 * Represents a single token in the source code
 */
export class Token {
  constructor(
    public readonly type: TokenType,
    public readonly value: string,
    public readonly line: number,
    public readonly column: number
  ) {}

  /**
   * Returns a string representation of the token
   */
  toString(): string {
    return `Token(${this.type}, '${this.value}', ${this.line}:${this.column})`;
  }

  /**
   * Checks if this token matches the given type
   */
  is(type: TokenType): boolean {
    return this.type === type;
  }

  /**
   * Checks if this token is one of the given types
   */
  isOneOf(types: TokenType[]): boolean {
    return types.includes(this.type);
  }
}

/**
 * Map of keywords to their token types
 */
export const KEYWORDS: Map<string, TokenType> = new Map([
  ['if', TokenType.IF],
  ['else', TokenType.ELSE],
  ['while', TokenType.WHILE],
  ['for', TokenType.FOR],
  ['function', TokenType.FUNCTION],
  ['return', TokenType.RETURN],
  ['let', TokenType.LET],
  ['const', TokenType.CONST],
  ['true', TokenType.TRUE],
  ['false', TokenType.FALSE],
  ['null', TokenType.NULL],
]);
