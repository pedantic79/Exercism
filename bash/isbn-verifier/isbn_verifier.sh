#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o errtrace
set -o pipefail

err_report() {
    echo "Error running '$1' [rc=$2] line $3 "
}

trap 'err_report $BASH_COMMAND $? $LINENO' ERR

isbn() {
    local orig="${1//-}"

    if [[ "${orig%%X}" =~ [^[:digit:]] ]]; then
        echo false
        exit 0
    fi

    local sum=0
    local i
    for ((i = 1; i <= ${#orig}; i++ )); do
        local pos=$((${#orig} - i))
        local digit=${orig:$pos:1}
        if [ "$digit" = 'X' ]; then
            sum=10
        else
            ((sum+=digit*i)) || true
        fi
    done

    if ! ((sum % 11)) && [ "$i" -eq 11 ]; then
        echo true
    else
        echo false
    fi
}

isbn "$@"
