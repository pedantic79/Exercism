export const hey = (message) => {
  const msg = message.trim();

  if (msg === '') {
    return 'Fine. Be that way!';
  } else {
    const isQuestion = msg.endsWith('?');
    const isYell = msg === msg.toUpperCase() && msg !== msg.toLowerCase();

    if (isQuestion) {
      return isYell ? "Calm down, I know what I'm doing!" : 'Sure.';
    } else {
      return isYell ? 'Whoa, chill out!' : 'Whatever.';
    }
  }
};
