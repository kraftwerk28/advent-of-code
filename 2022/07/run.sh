#!/bin/bash
wd=$(dirname "$0")
gcc -Wall -O3 -o "$wd/main" "$wd/main.c"
exec "$wd/main"
