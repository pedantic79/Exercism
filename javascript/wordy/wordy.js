export class ArgumentError {
  constructor(message) {
    this.name = "ArgumentError";
    this.message = message;
  }
}
export class WordProblem {
  constructor(input) {
    this.input = input;
  }

  answer() {
    if (!this.input.endsWith("?")) {
      throw new ArgumentError(`parse error: ${this.input} does not end with ?`);
    }

    const tokens = this.input
      .replace(/\?$/, "")
      .split(/ +/)
      .reverse();

    checkToken("What", tokens.pop());
    checkToken("is", tokens.pop());
    let total = parseNum(tokens.pop());

    while (tokens.length > 0) {
      const operation = parseOp(tokens);
      const number = parseNum(tokens.pop());
      total = operation(total, number);
    }
    return total;
  }
}

const checkToken = (expected, actual) => {
  if (expected !== actual) {
    throw new ArgumentError(
      `parse error: '${expected}' is expected, found ${actual}`
    );
  }
};

const parseNum = token => {
  const number = token.match(/^(-?\d+)$/);
  if (number === undefined) {
    throw new ArgumentError(`parse error: invalid number ${token}`);
  }
  return Number(number[1]);
};

const OPERATIONS = {
  plus: (total, value) => total + value,
  minus: (total, value) => total - value,
  multiplied: (total, value) => total * value,
  divided: (total, value) => total / value
};

const parseOp = tokens => {
  const token = tokens.pop();
  if (token === "multiplied" || token === "divided") {
    checkToken("by", tokens.pop());
  }

  const operation = OPERATIONS[token];
  if (operation === undefined) {
    throw new ArgumentError(`parse error: unknown operation ${token}`);
  }

  return operation;
};
