#!/bin/bash
infile="input/1.txt"
(( result = 0 ))
while read line; do
	if [[ -z $prev ]]; then
		prev=$line
	else
		(( line > prev && result++, prev = line ))
	fi
done < "$infile"
echo "part 1: $result"

read -d '\n' -r a b c < <(head -3 $infile)
(( result = 0, sum = a + b + c ))
while read line; do
	(( a = b, b = c, c = line ))
	(( temp = a + b + c ))
	(( temp > sum && result++ ))
	(( sum = temp ))
done < <(tail -n +3 "$infile")
echo "part 2: $result"
