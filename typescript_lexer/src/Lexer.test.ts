import { Lexer } from './Lexer';
import { TokenType } from './Token';
import { LexerError } from './LexerError';

describe('Lexer', () => {
  describe('Basic Tokenization', () => {
    test('should tokenize numbers', () => {
      const lexer = new Lexer('42 3.14');
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(3); // 2 numbers + EOF
      expect(tokens[0].type).toBe(TokenType.NUMBER);
      expect(tokens[0].value).toBe('42');
      expect(tokens[1].type).toBe(TokenType.NUMBER);
      expect(tokens[1].value).toBe('3.14');
      expect(tokens[2].type).toBe(TokenType.EOF);
    });

    test('should tokenize strings with double quotes', () => {
      const lexer = new Lexer('"hello world"');
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(2); // 1 string + EOF
      expect(tokens[0].type).toBe(TokenType.STRING);
      expect(tokens[0].value).toBe('hello world');
    });

    test('should tokenize strings with single quotes', () => {
      const lexer = new Lexer("'hello world'");
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(2);
      expect(tokens[0].type).toBe(TokenType.STRING);
      expect(tokens[0].value).toBe('hello world');
    });

    test('should handle escape sequences in strings', () => {
      const lexer = new Lexer('"hello\\nworld\\ttab"');
      const tokens = lexer.tokenize();

      expect(tokens[0].value).toBe('hello\nworld\ttab');
    });

    test('should tokenize identifiers', () => {
      const lexer = new Lexer('foo bar_baz _underscore');
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(4); // 3 identifiers + EOF
      expect(tokens[0].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[0].value).toBe('foo');
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[1].value).toBe('bar_baz');
      expect(tokens[2].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[2].value).toBe('_underscore');
    });
  });

  describe('Keywords', () => {
    test('should tokenize keywords', () => {
      const lexer = new Lexer('if else while for function return');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.IF);
      expect(tokens[1].type).toBe(TokenType.ELSE);
      expect(tokens[2].type).toBe(TokenType.WHILE);
      expect(tokens[3].type).toBe(TokenType.FOR);
      expect(tokens[4].type).toBe(TokenType.FUNCTION);
      expect(tokens[5].type).toBe(TokenType.RETURN);
    });

    test('should tokenize boolean and null keywords', () => {
      const lexer = new Lexer('true false null');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.TRUE);
      expect(tokens[1].type).toBe(TokenType.FALSE);
      expect(tokens[2].type).toBe(TokenType.NULL);
    });

    test('should tokenize let and const', () => {
      const lexer = new Lexer('let const');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LET);
      expect(tokens[1].type).toBe(TokenType.CONST);
    });
  });

  describe('Operators', () => {
    test('should tokenize arithmetic operators', () => {
      const lexer = new Lexer('+ - * / %');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.PLUS);
      expect(tokens[1].type).toBe(TokenType.MINUS);
      expect(tokens[2].type).toBe(TokenType.MULTIPLY);
      expect(tokens[3].type).toBe(TokenType.DIVIDE);
      expect(tokens[4].type).toBe(TokenType.MODULO);
    });

    test('should tokenize comparison operators', () => {
      const lexer = new Lexer('< > <= >= == !=');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LESS_THAN);
      expect(tokens[1].type).toBe(TokenType.GREATER_THAN);
      expect(tokens[2].type).toBe(TokenType.LESS_EQUAL);
      expect(tokens[3].type).toBe(TokenType.GREATER_EQUAL);
      expect(tokens[4].type).toBe(TokenType.EQUALS);
      expect(tokens[5].type).toBe(TokenType.NOT_EQUALS);
    });

    test('should tokenize logical operators', () => {
      const lexer = new Lexer('&& || !');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.AND);
      expect(tokens[1].type).toBe(TokenType.OR);
      expect(tokens[2].type).toBe(TokenType.NOT);
    });

    test('should tokenize assignment operator', () => {
      const lexer = new Lexer('=');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.ASSIGN);
    });
  });

  describe('Delimiters', () => {
    test('should tokenize parentheses and braces', () => {
      const lexer = new Lexer('( ) { } [ ]');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LPAREN);
      expect(tokens[1].type).toBe(TokenType.RPAREN);
      expect(tokens[2].type).toBe(TokenType.LBRACE);
      expect(tokens[3].type).toBe(TokenType.RBRACE);
      expect(tokens[4].type).toBe(TokenType.LBRACKET);
      expect(tokens[5].type).toBe(TokenType.RBRACKET);
    });

    test('should tokenize punctuation', () => {
      const lexer = new Lexer('; , .');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.SEMICOLON);
      expect(tokens[1].type).toBe(TokenType.COMMA);
      expect(tokens[2].type).toBe(TokenType.DOT);
    });
  });

  describe('Comments', () => {
    test('should skip line comments', () => {
      const lexer = new Lexer('let x = 10; // this is a comment\nlet y = 20;');
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(11); // let x = 10 ; let y = 20 ; EOF
      expect(tokens[0].type).toBe(TokenType.LET);
      expect(tokens[0].value).toBe('let');
      expect(tokens[5].type).toBe(TokenType.LET);
      expect(tokens[5].value).toBe('let');
    });

    test('should skip block comments', () => {
      const lexer = new Lexer('let x /* comment */ = 10;');
      const tokens = lexer.tokenize();

      expect(tokens).toHaveLength(6); // let x = 10 ; EOF
      expect(tokens[0].type).toBe(TokenType.LET);
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[2].type).toBe(TokenType.ASSIGN);
    });

    test('should handle multi-line block comments', () => {
      const lexer = new Lexer(`let x = 10;
/* This is a
   multi-line
   comment */
let y = 20;`);
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LET);
      expect(tokens[5].type).toBe(TokenType.LET);
    });
  });

  describe('Complex Expressions', () => {
    test('should tokenize a simple function', () => {
      const source = `function add(a, b) {
  return a + b;
}`;
      const lexer = new Lexer(source);
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.FUNCTION);
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[1].value).toBe('add');
      expect(tokens[2].type).toBe(TokenType.LPAREN);
      expect(tokens[3].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[3].value).toBe('a');
      expect(tokens[4].type).toBe(TokenType.COMMA);
    });

    test('should tokenize array initialization', () => {
      const lexer = new Lexer('const arr = [1, 2, 3];');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.CONST);
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[2].type).toBe(TokenType.ASSIGN);
      expect(tokens[3].type).toBe(TokenType.LBRACKET);
      expect(tokens[4].type).toBe(TokenType.NUMBER);
      expect(tokens[5].type).toBe(TokenType.COMMA);
      // const arr = [ 1 , 2 , 3 ] ;
      // 0     1   2 3 4 5 6 7 8 9 10 11
      expect(tokens[9].type).toBe(TokenType.RBRACKET);
    });

    test('should tokenize conditional statement', () => {
      const source = `if (x > 10) {
  return true;
}`;
      const lexer = new Lexer(source);
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.IF);
      expect(tokens[1].type).toBe(TokenType.LPAREN);
      expect(tokens[2].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[3].type).toBe(TokenType.GREATER_THAN);
      expect(tokens[4].type).toBe(TokenType.NUMBER);
    });
  });

  describe('Error Handling', () => {
    test('should throw error for unterminated string', () => {
      const lexer1 = new Lexer('"unterminated');
      const lexer2 = new Lexer('"unterminated');

      expect(() => lexer1.tokenize()).toThrow(LexerError);
      expect(() => lexer2.tokenize()).toThrow('Unterminated string literal');
    });

    test('should throw error for invalid character', () => {
      const lexer1 = new Lexer('let x = 10 @ 5;');
      const lexer2 = new Lexer('let x = 10 @ 5;');

      expect(() => lexer1.tokenize()).toThrow(LexerError);
      expect(() => lexer2.tokenize()).toThrow('Unexpected character');
    });

    test('should throw error for single ampersand', () => {
      const lexer = new Lexer('let x = a & b;');

      expect(() => lexer.tokenize()).toThrow(LexerError);
    });

    test('should throw error for single pipe', () => {
      const lexer = new Lexer('let x = a | b;');

      expect(() => lexer.tokenize()).toThrow(LexerError);
    });

    test('should throw error for unterminated block comment', () => {
      const lexer1 = new Lexer('/* unterminated comment');
      const lexer2 = new Lexer('/* unterminated comment');

      expect(() => lexer1.tokenize()).toThrow(LexerError);
      expect(() => lexer2.tokenize()).toThrow('Unterminated block comment');
    });

    test('should provide line and column information in errors', () => {
      const lexer = new Lexer('let x = 10;\nlet y = @;');

      try {
        lexer.tokenize();
        fail('Should have thrown an error');
      } catch (error) {
        expect(error).toBeInstanceOf(LexerError);
        if (error instanceof LexerError) {
          expect(error.line).toBe(2);
          expect(error.column).toBeGreaterThan(0);
        }
      }
    });
  });

  describe('Token Methods', () => {
    test('Token.is() should check token type', () => {
      const lexer = new Lexer('let');
      const tokens = lexer.tokenize();

      expect(tokens[0].is(TokenType.LET)).toBe(true);
      expect(tokens[0].is(TokenType.CONST)).toBe(false);
    });

    test('Token.isOneOf() should check multiple types', () => {
      const lexer = new Lexer('let const if');
      const tokens = lexer.tokenize();

      expect(tokens[0].isOneOf([TokenType.LET, TokenType.CONST])).toBe(true);
      expect(tokens[1].isOneOf([TokenType.LET, TokenType.CONST])).toBe(true);
      expect(tokens[2].isOneOf([TokenType.LET, TokenType.CONST])).toBe(false);
    });

    test('Token.toString() should return formatted string', () => {
      const lexer = new Lexer('let');
      const tokens = lexer.tokenize();

      const str = tokens[0].toString();
      expect(str).toContain('Token');
      expect(str).toContain('LET');
      expect(str).toContain('let');
    });
  });

  describe('Recursion Demonstration', () => {
    test('should handle recursive digit consumption', () => {
      const lexer = new Lexer('123456789');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.NUMBER);
      expect(tokens[0].value).toBe('123456789');
    });

    test('should handle decimal numbers with recursion', () => {
      const lexer = new Lexer('3.14159265');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.NUMBER);
      expect(tokens[0].value).toBe('3.14159265');
    });
  });
});
