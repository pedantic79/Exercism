export const secretHandshake = secret => {
  if (isNaN(secret)) {
    throw Error("Handshake must be a number");
  }

  const shake = ["wink", "double blink", "close your eyes", "jump"].filter(
    (_, idx) => secret & (1 << idx)
  );

  return secret & 16 ? shake.reverse() : shake;
};
