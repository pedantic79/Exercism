const SECRET_HANDSHAKE_OPERATIONS = {
  0b0001: "wink",
  0b0010: "double blink",
  0b0100: "close your eyes",
  0b1000: "jump",
};

const REVERSE_HANDSHAKE_KEY = 0b10000;

export default class Handshake {
  constructor(private readonly secret: number) {}

  commands(): string[] {
    const shake = Object.entries(SECRET_HANDSHAKE_OPERATIONS)
      .sort()
      .filter(([handshakeType, _]) => this.secret & Number(handshakeType))
      .map(([_, handShakeOperation]) => handShakeOperation);

    return this.secret & REVERSE_HANDSHAKE_KEY ? shake.reverse() : shake;
  }
}
