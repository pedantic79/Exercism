export const value = (colors) => {
  const COLORS = [
    'black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet',
    'grey', 'white'
  ];

  return parseInt(colors.reduce((accumulator, value) => {
    return accumulator + COLORS.indexOf(value).toString();
  }, ''));
};
