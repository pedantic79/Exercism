#!/usr/bin/env bash

set -o errexit
set -o nounset


is_question() {
    [ "${1: -1:1}" = "?" ]
}

is_yell() {
    local upper=${1^^} #uppercase
    local lower=${1,,} #lowerspace

    [ "$upper" = "$1" ] && [ "$upper" != "$lower" ]
}

main() {
    remark="${1-}"                   #Handle no params
    remark="$(echo -e "$remark")"     #Is there a non-$() way to do this?
    remark="${remark//[[:space:]]/}" #Remove all whitespace
    if [ "$remark" = "" ]; then
        echo "Fine. Be that way!"
    elif is_yell "$remark"; then
        if is_question "$remark"; then
            echo "Calm down, I know what I'm doing!"
        else
            echo "Whoa, chill out!"
        fi
    elif is_question "$remark"; then
        echo "Sure."
    else
        echo "Whatever."
    fi
}

main "$@"
