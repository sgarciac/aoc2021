#!/bin/bash
forward=$(cat input | grep forward | cut -d" " -f 2 | (sum=0; while read number; do (( sum += number )); done; echo $sum))
up=$(cat input | grep up | cut -d" " -f 2 | (sum=0; while read number; do (( sum += number )); done; echo $sum))
down=$(cat input | grep down | cut -d" " -f 2 | (sum=0; while read number; do (( sum += number )); done; echo $sum))
echo $((forward * (down - up)))
