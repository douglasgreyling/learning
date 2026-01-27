import { promises as fs } from 'fs';
import { join } from 'path';

/**
 * Asynchronous file reader utility
 * Demonstrates async/await functionality
 */
export class FileReader {
  /**
   * Asynchronously reads a file and returns its contents
   */
  static async readFile(filePath: string): Promise<string> {
    try {
      const content = await fs.readFile(filePath, 'utf-8');
      return content;
    } catch (error) {
      if (error instanceof Error) {
        throw new Error(`Failed to read file ${filePath}: ${error.message}`);
      }
      throw error;
    }
  }

  /**
   * Asynchronously reads multiple files
   * Demonstrates parallel async operations
   */
  static async readMultipleFiles(filePaths: string[]): Promise<Map<string, string>> {
    const results = new Map<string, string>();

    // Read all files in parallel using Promise.all
    const filePromises = filePaths.map(async (filePath) => {
      const content = await FileReader.readFile(filePath);
      return { filePath, content };
    });

    const fileResults = await Promise.all(filePromises);

    // Store results in a map
    fileResults.forEach(({ filePath, content }) => {
      results.set(filePath, content);
    });

    return results;
  }

  /**
   * Gets the full path for an example file
   */
  static getExamplePath(filename: string): string {
    return join(__dirname, '..', 'examples', filename);
  }

  /**
   * Lists all example files asynchronously
   */
  static async listExampleFiles(): Promise<string[]> {
    const examplesDir = join(__dirname, '..', 'examples');
    try {
      const files = await fs.readdir(examplesDir);
      return files.filter(file => file.endsWith('.txt'));
    } catch (error) {
      if (error instanceof Error) {
        throw new Error(`Failed to list example files: ${error.message}`);
      }
      throw error;
    }
  }
}
