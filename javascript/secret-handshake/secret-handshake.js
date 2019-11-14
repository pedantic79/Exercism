const SECRET_HANDSHAKE_OPERATIONS = [
  "wink",
  "double blink",
  "close your eyes",
  "jump"
];

export const secretHandshake = secret => {
  if (typeof secret !== "number") {
    throw Error("Handshake must be a number");
  }

  const shake = SECRET_HANDSHAKE_OPERATIONS.filter(
    (_, idx) => secret & (1 << idx)
  );

  return secret & 16 ? shake.reverse() : shake;
};
