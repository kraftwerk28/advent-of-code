#!/bin/bash
set -e
polyc -o main main.sml
exec ./main
