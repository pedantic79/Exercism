export default class Bob {
  hey(message: string): string {
    const msg = message.trim();

    if (msg === "") {
      return "Fine. Be that way!";
    } else {
      const question = msg.endsWith("?");
      const yell = isYell(msg);

      if (question) {
        return yell ? "Calm down, I know what I'm doing!" : "Sure.";
      } else {
        return yell ? "Whoa, chill out!" : "Whatever.";
      }
    }
  }
}

function isYell(msg: string): boolean {
  // Regex: Matches at least one uppercase, but no lowercase characters
  const ATLEAST_ONE_UPPERCASE = /^[^a-z]*[A-Z]+[^a-z]*$/;
  return ATLEAST_ONE_UPPERCASE.test(msg);

  // return msg === msg.toUpperCase() && msg !== msg.toLowerCase();
}

// This should really go in another file, but I didn't want to bother introducing
// a dependency like rewire to test the unexported function
describe("isYell", function () {
  it("all uppercase", () => expect(isYell("HELLO")).toBeTruthy());
  it("no uppercase or lowercase", () => expect(isYell("!@#@!")).toBeFalsy());
  it("one uppercase", () => expect(isYell("!@#@!A")).toBeTruthy());
  it("one lowercase", () => expect(isYell("!@#@!a")).toBeFalsy());
});
