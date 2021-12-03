#!/bin/zsh
infile=${1:-"input/02.txt"}
forward_re="^forward ([0-9]+)"
down_re="^down ([0-9]+)"
up_re="^up ([0-9]+)"

hor=0
depth=0
while read line; do
	if [[ $line =~ $forward_re ]]; then
		(( hor += ${match[1]} ))
	elif [[ $line =~ $down_re ]]; then
		(( depth += ${match[1]} ))
	elif [[ $line =~ $up_re ]]; then
		(( depth -= ${match[1]} ))
	fi
done < $infile
echo "part 1: $(( hor * depth ))"

hor=0
depth=0
aim=0
while read line; do
	if [[ $line =~ $forward_re ]]; then
		(( hor += ${match[1]} ))
		(( depth += aim * ${match[1]} ))
	elif [[ $line =~ $down_re ]]; then
		(( aim += ${match[1]} ))
	elif [[ $line =~ $up_re ]]; then
		(( aim -= ${match[1]} ))
	fi
done < $infile
echo "part 2: $(( hor * depth ))"
