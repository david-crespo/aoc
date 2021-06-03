#!/bin/bash

touch "input/day$1.txt"
sed "s/day[0-9][0-9]/day$1/g" src/template.rs > "src/day$1.rs"