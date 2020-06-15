#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
  local input=$1

  if [ "$input" = "total" ]; then
    local -i t=0
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
