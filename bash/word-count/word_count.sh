#!/usr/bin/env bash

set -o errexit
# set -o nounset

main() {
    local line="$1"

    IFS=$', .' read -r -a words <<< "$line"

    declare -A freq

    local word
    for word in "${words[@]}"; do
        word=${word//[\^\%\$\&\@\!\:]/}
        word=${word,,}
        if [ "${word:0:1}" = "'" ] && [ "${word:${#word}-1:1}" = "'" ]; then
            word=${word:1:${#word} - 2}
        fi
        freq[$word]=$(("${freq[$word]}" + 1))
    done

    for word in "${!freq[@]}"; do
        echo "$word: ${freq[$word]}"
    done
}

main "$@"
