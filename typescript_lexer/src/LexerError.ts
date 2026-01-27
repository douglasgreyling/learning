/**
 * Custom error class for lexer errors
 * Demonstrates exception handling in TypeScript
 */
export class LexerError extends Error {
  constructor(
    message: string,
    public readonly line: number,
    public readonly column: number,
    public readonly character?: string
  ) {
    super(`Lexer Error at ${line}:${column}: ${message}`);
    this.name = 'LexerError';

    // Maintains proper stack trace for where our error was thrown
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, LexerError);
    }
  }

  /**
   * Returns a formatted error message for display
   */
  getFormattedMessage(): string {
    const charInfo = this.character ? ` (found '${this.character}')` : '';
    return `[Line ${this.line}, Column ${this.column}] ${this.message}${charInfo}`;
  }
}
