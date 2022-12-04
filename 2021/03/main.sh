#!/bin/bash

lines=()
while read -r line; do
	lines+=("$line")
done

nlines=0
declare -a nbits gamma_arr sigma_arr
for line in "${lines[@]}"; do
	(( nlines++ ))
	for (( i = 0; i < ${#line}; i++ )); do
		(( ${line:$i:1} == 1 && nbits[i]++ ))
	done
done
for (( i = 0; i < ${#nbits[@]}; i++ )); do
	if (( nbits[i] > nlines / 2 )); then
		(( gamma_arr[i] = 1, sigma_arr[i] = 0 ))
	else
		(( gamma_arr[i] = 0, sigma_arr[i] = 1 ))
	fi
done
part1=$(( 2#$(printf '%s' "${gamma_arr[*]}") * 2#$(printf '%s' "${sigma_arr[*]}") ))
echo "$part1"

filtered=("${lines[@]}")
nth_bit=0
n_ones=0
while (( ${#filtered[@]} > 1 )); do
	n_ones=0
	temp=()
	for (( i = 0; i < ${#filtered[@]}; i++ )); do
		(( (${filtered[i]:$nth_bit:1} == 1) && n_ones++ ))
	done
	(( filter_bit = (n_ones >= ${#filtered[@]}.0 / 2) ? 1 : 0 ))
	for el in "${filtered[@]}"; do
		if [[ ${el:$nth_bit:1} == "$filter_bit" ]]; then
			temp+=("$el")
		fi
	done
	filtered=("${temp[@]}")
	(( nth_bit++ ))
done
oxygen=${filtered[0]}
filtered=("${lines[@]}")
nth_bit=0
n_ones=0
while (( ${#filtered[@]} > 1 )); do
	n_ones=0
	temp=()
	for (( i = 0; i < ${#filtered[@]}; i++ )); do
		(( (${filtered[i]:$nth_bit:1} == 1) && n_ones++ ))
	done
	(( filter_bit = (n_ones >= ${#filtered[@]}.0 / 2) ? 0 : 1 ))
	for el in "${filtered[@]}"; do
		if [[ ${el:$nth_bit:1} == "$filter_bit" ]]; then
			temp+=("$el")
		fi
	done
	filtered=("${temp[@]}")
	(( nth_bit++ ))
done
co2=${filtered[0]}
echo "$(( 2#$oxygen * 2#$co2 ))"
