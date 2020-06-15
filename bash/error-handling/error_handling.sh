#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    if [ $# -eq 0 ] || [ $# -gt 1 ]; then
        echo "Usage: $0 <person>"
        exit 1
    else
        echo "Hello, $1"
    fi
}

main "$@"
