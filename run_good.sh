#!/bin/bash

for file in good/*.szk
do
    echo "Running $file"
    ./interpreter $file
    echo "-------------------"
done