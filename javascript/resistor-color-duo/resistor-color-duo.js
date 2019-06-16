export const value = (colors) => parseInt(colors.reduce(
    (accumulator, value) => accumulator + COLORS.indexOf(value).toString(),
    ''));


const COLORS = [
  'black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet',
  'grey', 'white'
];
