#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local number="${1//[(). +-]}"

    if  [[ $number =~ [^[:digit:]] ]] || \
        [[ $number =~ ^[2-9][[:digit:]]{10}$ ]] || \
        [[ $number =~ ^0 ]] || \
        [ "${#number}" -lt 10 ] || \
        [ "${#number}" -gt 11 ] || \
        {
            [ "${#number}" -eq 10 ] && \
            {
                [ "${number:0:1}" = "1" ] || \
                [ "${number:3:1}" = "0" ] || \
                [ "${number:3:1}" = "1" ]
            }
        }
    then
        echo "Invalid number.  [1]NXX-NXX-XXXX N=2-9, X=0-9"
        exit 1
    elif [[ $number =~ ^1[[:digit:]]{10}$ ]]; then
        echo "${number:1}"
    else
        echo "$number"
    fi
}

main "$@"
