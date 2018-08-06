#!/bin/bash

set -o errexit

main() {
    local input=$1

    echo "One for ${input:-you}, one for me."
}

main "$@"
