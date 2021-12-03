#!/bin/bash
h=0
d=0
aim=0
while read line; do
    command=$(echo $line | cut -d" " -f1)
    val=$(echo $line | cut -d" " -f2)
    if [ "$command" = "forward" ]; then
        ((h+=val))
        ((d+=aim*val))
    elif [ "$command" = "up" ]; then
        ((aim-=val))
    else
        ((aim+=val))
    fi
done < input
echo $((d*h))
