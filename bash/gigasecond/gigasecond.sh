#!/usr/bin/env bash

# This requires gnu date
# Making this work with bsd date would take too much time
if hash gdate 2>/dev/null; then
    cmd="gdate"
else
    cmd="date"
fi

d="$* +1000000000 second"
echo $("$cmd" --date="$d" -u)
