#!/bin/zsh
infile=${1:-"input/03.txt"}

setopt ksh_arrays
nlines=0
declare -a nbits gamma_arr sigma_arr
while read line; do
	(( nlines++ ))
	for (( i = 0; i < ${#line}; i++ )); do
		(( ${line:$i:1} == 1 && nbits[i]++ ))
	done
done < $infile
for (( i = 0; i < ${#nbits[@]}; i++ )); do
	if (( nbits[i] > nlines / 2 )); then
		(( gamma_arr[i] = 1, sigma_arr[i] = 0 ))
	else
		(( gamma_arr[i] = 0, sigma_arr[i] = 1 ))
	fi
done
part1=$(( 2#$(printf '%s' ${gamma_arr[*]}) * 2#$(printf '%s' ${sigma_arr[*]}) ))
echo "part 1: $part1"

filtered=($(cat $infile | xargs))
nth_bit=0
n_ones=0
while (( ${#filtered[@]} > 1 )); do
	n_ones=0
	temp=()
	for (( i = 0; i < ${#filtered[@]}; i++ )); do
		(( (${filtered[i]:$nth_bit:1} == 1) && n_ones++ ))
	done
	(( filter_bit = (n_ones >= ${#filtered[@]}.0 / 2) ? 1 : 0 ))
	for el in ${filtered[@]}; do
		if [[ ${el:$nth_bit:1} == $filter_bit ]]; then
			temp+=($el)
		fi
	done
	filtered=(${temp[@]})
	(( nth_bit++ ))
done
oxygen=${filtered[0]}
filtered=($(cat $infile | xargs))
nth_bit=0
n_ones=0
while (( ${#filtered[@]} > 1 )); do
	n_ones=0
	temp=()
	for (( i = 0; i < ${#filtered[@]}; i++ )); do
		(( (${filtered[i]:$nth_bit:1} == 1) && n_ones++ ))
	done
	(( filter_bit = (n_ones >= ${#filtered[@]}.0 / 2) ? 0 : 1 ))
	for el in ${filtered[@]}; do
		if [[ ${el:$nth_bit:1} == $filter_bit ]]; then
			temp+=($el)
		fi
	done
	filtered=(${temp[@]})
	(( nth_bit++ ))
done
co2=${filtered[0]}
echo "part 2: $(( 2#$oxygen * 2#$co2 ))"
