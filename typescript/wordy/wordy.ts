/*
 * @prettier
 */

/* eslint @typescript-eslint/no-use-before-define: "off" */
export class ArgumentError {
  private readonly name = "ArgumentError";
  constructor(private readonly message: string) {}
}

export class WordProblem {
  private static readonly PREFIX = ["What", "is"];
  private static readonly OPERATIONS_VARIANTS = [
    ["plus"],
    ["minus"],
    ["multiplied", "by"],
    ["divided", "by"],
  ];
  private static readonly ENDS_WITH = "?";

  private static readonly OPERATIONS: { [index: string]: OperationFn } = {
    plus: (total, value) => total + value,
    minus: (total, value) => total - value,
    multiplied: (total, value) => total * value,
    divided: (total, value) => total / value,
  };

  private tokens: Tokenizer;

  constructor(private readonly input: string) {
    this.tokens = new Tokenizer(WordProblem.trimEnd(input, "?"));
  }

  private static trimEnd(input: string, pattern: string): string {
    const last = input.length - 1;
    return input[last] == pattern ? input.substring(0, last) : input;
  }

  parseOp(): OperationFn {
    const token = this.tokens.checkMultipleVariants(
      WordProblem.OPERATIONS_VARIANTS
    );

    const operation = WordProblem.OPERATIONS[token];
    if (!operation) {
      throw new ArgumentError(`parse error: unknown operation "${token}"`);
    }

    return operation;
  }

  checkEnding(input: string): void {
    if (!input.endsWith(WordProblem.ENDS_WITH)) {
      throw new ArgumentError(
        `parse error: ${input} does not end with ${WordProblem.ENDS_WITH}`
      );
    }
  }

  answer(): number {
    this.tokens.checkTokens(WordProblem.PREFIX);

    let total = this.tokens.parseNum();
    while (!this.tokens.isEmpty()) {
      const operation = this.parseOp();
      const number = this.tokens.parseNum();
      total = operation(total, number);
    }

    this.checkEnding(this.input);

    return total;
  }
}

type OperationFn = (total: number, value: number) => number;

class Tokenizer {
  private tokens: string[];

  constructor(input: string) {
    this.tokens = input.split(" ");
  }

  shift(): string {
    return this.tokens.shift() || "";
  }

  shiftFirst(count: number): string {
    const token = this.shift();
    for (let index = 1; index < count; index++) {
      this.shift();
    }

    return token;
  }

  isEmpty(): boolean {
    return this.tokens.length === 0;
  }

  private checkVariant(expected: string[]): string | undefined {
    for (let index = 0; index < expected.length; index++) {
      if (this.tokens[index] !== expected[index]) {
        return undefined;
      }
    }

    return this.shiftFirst(expected.length);
  }

  checkToken(expected: string): void {
    const token = this.shift();

    if (token !== expected) {
      throw new ArgumentError(
        `parse error: '${expected}' is expected, found ${token}`
      );
    }
  }

  checkTokens(expected: string[]): void {
    if (!this.checkVariant(expected)) {
      throw new ArgumentError(`parse error: ${expected} not found`);
    }
  }

  checkMultipleVariants(variants: string[][]): string {
    for (const variant of variants) {
      const token = this.checkVariant(variant);
      if (token) {
        return token;
      }
    }

    return this.shift();
  }

  parseNum(): number {
    const token = this.shift();

    const number = parseInt(token);
    if (isNaN(number)) {
      throw new ArgumentError(`parse error: invalid number ${token}`);
    }
    return number;
  }
}
