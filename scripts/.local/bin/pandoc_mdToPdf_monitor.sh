#!/bin/sh
# Usage: ./file_monitor.sh file_name
f=$1
cmd="pandoc -s -o "/tmp/${f}.pdf" "${f}""
tmpf="$(mktemp /tmp/onchange.XXXXX)"

# Kill previous Instances
# TODO function for toggle 
for pid in $(pidof -x "${0}"); do
    if [ $pid != $$ ]; then
        kill -9 $pid
    fi 
done

# Monitor the file for changes
cp "$f" "$tmpf"
trap "rm ""${tmpf}""; exit 1" 2
while : ; do
    if [ "$f" -nt "$tmpf" ]; then
        cp "$f" "$tmpf"
        $cmd
    fi
    sleep 2
done
