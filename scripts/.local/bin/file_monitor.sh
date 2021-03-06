#!/bin/sh
# Usage: ./file_monitor.sh file_name command_to_run
f=$1
shift
cmd="$*"
tmpf="$(mktemp /tmp/onchange.XXXXX)"
cp "$f" "$tmpf"
trap "rm ""${tmpf}""; exit 1" 2
while : ; do
    if [ "$f" -nt "$tmpf" ]; then
        cp "$f" "$tmpf"
        $cmd
    fi
    sleep 2
done
