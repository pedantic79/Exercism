#!/usr/bin/env bash

# This is a bash script template in order to help you quick start any script.
# It contains some sensible defaults, you can learn more by visiting:
# https://google.github.io/styleguide/shell.xml

# This option will make the script exit when there is an error
set -o errexit
# This option will make the script exit when it tries to use an unset variable
set -o nounset

main() {
  # A string variable containing only the FIRST argument passed to the script,
  # you can use input=$@ to get a string with ALL arguments
  input=$1

  if [ "$input" = "total" ]; then
    local t=0
    for i in $(seq 1 63); do
      local sq
      sq=$(main "$i")
      ((t+=sq))
    done
    # This overflows in pure bash. Falling back to bc to add square 64
    echo "$t + $(main 64)" | bc
  elif [ "$input" -le 0 ] || [ "$input" -gt 64 ]; then
    echo "Error: invalid input"
    exit 1
  elif [ "$input" -lt 64 ]; then
    echo $((2 ** (input - 1)))
  else
    # This overflows in pure bash. Falling back to bc
    echo "2 ^ 63" | bc
  fi
}

# Calls the main function passing all the arguments to it via '$@'
# The argument is in quotes to prevent whitespace issues
main "$@"
