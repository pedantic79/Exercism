#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    if [ $# -eq 0 ] || [ $# -gt 1 ]; then
        echo "Usage: ./error_handling <greetee>"
        exit 1
    else
        echo "Hello, $1"
    fi
}

main "$@"
