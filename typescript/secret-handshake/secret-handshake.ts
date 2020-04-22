const SECRET_HANDSHAKE_OPERATIONS = [
  "wink",
  "double blink",
  "close your eyes",
  "jump",
];

export default class Handshake {
  constructor(private readonly secret: number) {}

  commands(): string[] {
    const shake = SECRET_HANDSHAKE_OPERATIONS.filter(
      (_, handshakeIndex) => this.secret & (1 << handshakeIndex)
    );

    return this.secret & (1 << SECRET_HANDSHAKE_OPERATIONS.length)
      ? shake.reverse()
      : shake;
  }
}
