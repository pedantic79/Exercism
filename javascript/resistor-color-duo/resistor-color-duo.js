export const value = (colors) => {
  return parseInt(colors.reduce((accumulator, value) => {
    return accumulator + COLORS.indexOf(value).toString();
  }, ''));
};

const COLORS = [
  'black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet',
  'grey', 'white'
];
