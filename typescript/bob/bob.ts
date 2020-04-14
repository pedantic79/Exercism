// Regex: Matches at least one uppercase
const ATLEAST_ONE_UPPERCASE = /[A-Z]/;

const isSilence = (msg: string): boolean => msg.trim().length === 0;
const isQuestion = (msg: string): boolean => msg.trimEnd().endsWith("?");
const isYell = (msg: string): boolean =>
  ATLEAST_ONE_UPPERCASE.test(msg) && msg === msg.toUpperCase();

export default class Bob {
  hey(message: string): string {
    const silence = isSilence(message);
    const yell = isYell(message);
    const question = isQuestion(message);
    const shoutedQuestion = yell && question;

    if (silence) return "Fine. Be that way!";
    if (shoutedQuestion) return "Calm down, I know what I'm doing!";
    if (question) return "Sure.";
    if (yell) return "Whoa, chill out!";
    return "Whatever.";
  }
}

// This should really go in another file, but I didn't want to bother introducing
// a dependency like rewire to test the unexported function
describe("isYell", function () {
  it("all uppercase", () => expect(isYell("HELLO")).toBeTruthy());
  it("no uppercase or lowercase", () => expect(isYell("!@#@!")).toBeFalsy());
  it("one uppercase", () => expect(isYell("!@#@!A")).toBeTruthy());
  it("one lowercase", () => expect(isYell("!@#@!a")).toBeFalsy());
});

describe("isSilence", function () {
  it("empty", () => expect(isSilence("")).toBeTruthy());
  it("only spaces", () => expect(isSilence("    ")).toBeTruthy());
  it("non-empty, with spaces", () => expect(isSilence("  a    ")).toBeFalsy());
});

describe("isQuestion", function () {
  it("empty", () => expect(isQuestion("")).toBeFalsy());
  it("ends with ?", () => expect(isQuestion("word?")).toBeTruthy());
  it("? with spaces", () => expect(isQuestion("word?   \t")).toBeTruthy());
});
