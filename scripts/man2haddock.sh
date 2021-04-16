#!/usr/bin/env bash

# USAGE:
#     man2haddock.sh <PATH>

readonly path="$1"
format="$2"
if [[ -z "$format" ]]; then
    format=haddock
fi
readonly format
echo "Converting to $format"

filename="$(basename "$path")"
stem="${filename%.*}"
stem="${stem%.*}"
ext="${filename##*.}"
echo "  from: $filename"

if [[ "$ext" == "gz" ]]; then
    gzip -c -d "$path" | pandoc --from=man --to="$format" --output="$stem"."$format"
elif [[ "123456789" == *"$ext"* ]]; then
    pandoc --from=man --to="$format" --output="$stem"."$format" "$path"
else
    echo "ERROR: The input file is not a man page."
    exit 1
fi
