#!/usr/bin/env bash

set -o errexit
set -o nounset

output() {
    if [ "$1" -eq 0 ]; then
        echo "true"
    else
        echo "false"
    fi
    exit 0
}

is_count() {
    local target="$1"
    shift

    local c
    c=$(count "$@")
    if { [ "$target" = "0" ] && [ "$c" -eq "0" ]; } || \
       { [ "$target" -gt "0" ] && [ "$c" -ge "$target" ]; } ; then
        output 0
    fi

    output 1
}

count() {
    local c=0
    if [ "$1" = "$2" ]; then
        ((c++)) || true
    fi

    if [ "$2" = "$3" ]; then
        ((c++)) || true
    fi

    if [ "$3" = "$1" ]; then
        ((c++)) || true
    fi

    echo "$c"
}

check_legal() {
    local sides=("$@")
    local i
    for ((i=0; i<${#sides}; i++)); do
        if [ "${sides[$i]}" = "0" ]; then
            output 1
        fi
    done

    if [[ "$1" =~ \. ]]; then
        # handle floating points
        if [ "$(bc -l <<< "$1+$2<=$3 && $1+$3<=$2 && $3+$2<=$1")" = "1" ]; then
            output 1
        fi
    else
        if [ $(($1 + $2)) -le "$3" ] || \
            [ $(($1 + $3)) -le "$2" ] || \
            [ $(($3 + $2)) -le "$1" ]; then

            output 1
        fi
    fi
}


main() {
    local cmd=$1
    shift

    check_legal "$@"
    if [ "$cmd" = "equilateral" ]; then
        is_count 3 "$@"
    elif [ "$cmd" = "isosceles" ]; then
        is_count 1 "$@"
    else
        is_count 0 "$@"
    fi

}

main "$@"
