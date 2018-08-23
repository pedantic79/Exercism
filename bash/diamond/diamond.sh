#!/usr/bin/env bash

set -o errexit
set -o nounset

ord() {
    local ch
    ch=$(printf "%d" "'$1")
    echo $((ch - 64))
}

chr() {
    local ascii
    local ch=$(($1 + 64))
    printf -v ascii "%o" "$ch"
    echo -ne "\\0$ascii"
}

print_repeat() {
    local num=$1
    if [ "$num" -gt 0 ]; then
        printf "%-${num}s" " "
    fi
}

print_line() {
    local w="$1"
    local c="$2"

    local side=$((w - c))

    print_repeat "$side"
    if [ "$c" -eq "1" ]; then
        chr "$c"
    else
        chr "$c"
        print_repeat $((2 * c - 3))
        chr "$c"
    fi
    print_repeat "$side"
    echo ""
}

main() {
    local letter="$1"

    local width
    width=$(ord "$letter")

    local i
    for ((i=1; i<=width; i++)); do
        print_line "$width" "$i"
    done

    if [ "$letter" != "A" ]; then
        ((i-=2)) || true
        for ((; i>=1; i--)); do
            print_line "$width" "$i"
        done
    fi
}

main "$@"
