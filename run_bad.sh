#!/bin/bash

for file in bad/*.szk
do
    echo "Running $file"
    ./interpreter $file
done

for file in bad/parse_errors/*.szk
do
    echo "Running $file"
    ./interpreter $file
done