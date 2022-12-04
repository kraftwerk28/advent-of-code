#!/bin/bash

lines=()
while read -r line; do
	lines+=("$line")
done

forward_re="^forward ([0-9]+)"
down_re="^down ([0-9]+)"
up_re="^up ([0-9]+)"

hor=0; depth=0
for line in "${lines[@]}"; do
	if [[ $line =~ $forward_re ]]; then
		(( hor += BASH_REMATCH[1] ))
	elif [[ $line =~ $down_re ]]; then
		(( depth += BASH_REMATCH[1] ))
	elif [[ $line =~ $up_re ]]; then
		(( depth -= BASH_REMATCH[1] ))
	fi
done
echo "$(( hor * depth ))"

hor=0; depth=0; aim=0
for line in "${lines[@]}"; do
	if [[ $line =~ $forward_re ]]; then
		(( hor += BASH_REMATCH[1] ))
		(( depth += aim * BASH_REMATCH[1] ))
	elif [[ $line =~ $down_re ]]; then
		(( aim += BASH_REMATCH[1] ))
	elif [[ $line =~ $up_re ]]; then
		(( aim -= BASH_REMATCH[1] ))
	fi
done
echo "$(( hor * depth ))"
