#!/usr/bin/env bash

set -o errexit
set -o nounset

ord() {
    local ch
    ch=$(printf "%d" "'$1")
    echo $((ch - 97))
}

chr() {
    local ascii
    local ch=$(($1 + 97))
    printf -v ascii "%o" "$ch"
    echo -e "\\0$ascii"
}

is_coprime () {
    local a=$1
    local b=$2

    local rem=1

    while [ "$rem" -ne 0 ]; do
        rem=$(( a % b ))
        a="$b"
        b="$rem"
    done

    [ "$a" -ne 1 ]
    return "$?"
}

encode() {
    local a=$1
    local b=$2
    local str=$3
    str=${str,,}
    str=${str//[^a-z0-9]/}

    local output=""
    local i
    for ((i=0; i<${#str}; i++)); do
        local x="${str:$i:1}"
        if [[ $x =~ [a-z] ]]; then
            x=$(ord "$x")
            encoded=$(( (a * x + b) % 26 ))
            output+=$(chr $encoded)
        else
            output+="$x"
        fi
    done

    local groups=()
    for ((i = 0; i < ${#output}; i+=5)); do
        groups+=("${output:$i:5}")
    done

    echo "${groups[*]}"
}

decode() {
    local a=$1
    local b=$2
    local str=${3// /}
    local i
    for ((i=0; i<${#str}; i++)); do
        local ch="${str:$i:1}"

        if [[ $ch =~ [[:alpha:]] ]]; then
            local x
            x="$(ord "$ch")"

            # Subtract b, then add 26 (undoes mod 26) until we divide evenly
            # and are a positive number
            ((x -= b)) || true
            while [ $((x % a)) != 0 ] || [ "$x" -lt 0 ]; do
                ((x += 26)) || true
            done
            local unmod=$((x / a))
            echo -n "$(chr $unmod)"
        else
            echo -n "$ch"
        fi
    done
    echo ""
}

check_input() {
    if is_coprime "$1" 26; then
        echo "a and m must be coprime."
        exit 1
    fi
}

main() {
    local op=$1
    shift

    check_input "$1"
    if [ "$op" = "encode" ]; then
        encode "$@"
    else
        decode "$@"
    fi
}

main "$@"
