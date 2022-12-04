#!/bin/bash
# tmpfile=$(mktemp)
lines=()
while read -r line; do
	lines+=("$line")
done

(( result = 0 ))

for line in "${lines[@]}"; do
	if [[ -z $prev ]]; then
		prev=$line
	else
		(( line > prev && result++, prev = line ))
	fi
done
echo "$result"

a="${lines[0]}"
b="${lines[1]}"
c="${lines[2]}"
(( result = 0, sum = a + b + c ))
for line in "${lines[@]}"; do
	(( a = b, b = c, c = line ))
	(( temp = a + b + c ))
	(( temp > sum && result++ ))
	(( sum = temp ))
done
echo "$result"
