export const hey = (message) => {
  const msg = message.trim();

  if (msg === '') {
    return 'Fine. Be that way!';
  } else {
    const isQuestion = msg.endsWith('?');
    // const isYell = msg === msg.toUpperCase() && msg !== msg.toLowerCase();

    // Regex: Matches at least one uppercase, but no lowercase characters
    const isYell = /^[^a-z]*[A-Z]+[^a-z]*$/.test(msg);

    if (isQuestion) {
      return isYell ? "Calm down, I know what I'm doing!" : 'Sure.';
    } else {
      return isYell ? 'Whoa, chill out!' : 'Whatever.';
    }
  }
};
