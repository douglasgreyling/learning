import { Lexer } from './Lexer';
import { LexerError } from './LexerError';
import { Token } from './Token';
import { FileReader } from './FileReader';

/**
 * Displays tokens in a formatted table
 */
function displayTokens(tokens: Token[]): void {
  console.log('\n' + '='.repeat(80));
  console.log('TOKENS');
  console.log('='.repeat(80));
  console.log(
    String.prototype.padEnd.call('TYPE', 20) +
    String.prototype.padEnd.call('VALUE', 25) +
    String.prototype.padEnd.call('POSITION', 15)
  );
  console.log('-'.repeat(80));

  tokens.forEach(token => {
    const type = String.prototype.padEnd.call(token.type, 20);
    const value = String.prototype.padEnd.call(`"${token.value}"`, 25);
    const position = `${token.line}:${token.column}`;

    console.log(type + value + position);
  });

  console.log('='.repeat(80));
  console.log(`Total tokens: ${tokens.length}\n`);
}

/**
 * Tokenizes source code and displays results
 */
function tokenizeSource(source: string, label: string): void {
  console.log(`\n${'*'.repeat(80)}`);
  console.log(`${label}`);
  console.log('*'.repeat(80));
  console.log('Source Code:');
  console.log('-'.repeat(80));
  console.log(source);
  console.log('-'.repeat(80));

  try {
    const lexer = new Lexer(source);
    const tokens = lexer.tokenize();
    displayTokens(tokens);
  } catch (error) {
    if (error instanceof LexerError) {
      console.error('\n❌ Lexer Error:');
      console.error(error.getFormattedMessage());
      console.error('');
    } else {
      console.error('\n❌ Unexpected Error:', error);
    }
  }
}

/**
 * Asynchronously reads a file and tokenizes its contents
 * Demonstrates async/await functionality
 */
async function tokenizeFile(filename: string, label: string): Promise<void> {
  try {
    console.log(`\n Reading file: ${filename}...`);
    const filePath = FileReader.getExamplePath(filename);
    const source = await FileReader.readFile(filePath);
    tokenizeSource(source, label);
  } catch (error) {
    if (error instanceof Error) {
      console.error(`\n❌ File Read Error: ${error.message}\n`);
    } else {
      console.error('\n❌ Unexpected Error:', error);
    }
  }
}

/**
 * Main function demonstrating all features
 */
async function main(): Promise<void> {
  console.log('\n' + '█'.repeat(80));
  console.log('TYPESCRIPT LEXER - DEMONSTRATION');
  console.log('█'.repeat(80));

  // Demo 1: Read and tokenize individual files sequentially
  console.log('\n' + '='.repeat(80));
  console.log('PART 1: SEQUENTIAL FILE READING (Async/Await)');
  console.log('='.repeat(80));

  await tokenizeFile('simple.txt', 'Example 1: Simple Variable Assignment');
  await tokenizeFile('conditional.txt', 'Example 2: Conditional Statement');
  await tokenizeFile('function.txt', 'Example 3: Function Definition');
}

// Run the main function
main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
