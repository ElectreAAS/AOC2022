#!/bin/fish

########################################################################
# The purpose of this script is to automate everything needed to move
# from one day to the next. As there are a few different tasks it's easy
# to forget one.
########################################################################

## Find yesterday's date
set yesterday (rg "day(\d+)\)\)" utils/dune -Nor '$1')
set today (math $yesterday + 1)
echo "Today's date is the $today"

if test $today -gt 25
    echo "Nothing new for today"
    exit 1
end

## Updating all the files to reference the new day

# utils/all.ml
sed -i "s/Day$yesterday);/&\n    (module Day$today);/ ; s/  ]/    \"$argv\";\n  ]/" utils/all.ml

# utils/dune
sed -i "s/day$yesterday/&\n  day$today/" utils/dune

## Create the new files

# Create directories
set dir_name "./day$today"
if test ! -d "$dir_name"
    echo "Creating directory $dir_name"
    mkdir -p "$dir_name"
end

# Create placeholder ocaml file
echo "Creating $dir_name/day$today.ml"
echo "let day _display _pool input_buffer =
  let line = Eio.Buf_read.line input_buffer in
  line" >$dir_name/day$today.ml

# Create dune file
echo "Creating $dir_name/dune"
echo "(library
 (name day$today)
 (libraries utils extensions))" >$dir_name/dune

# Get the needed session cookie
set cookie $AOC2022_SESSION
if test -z "$cookie"
    echo "Session cookie not found!"
    exit 1
end

# Fetch input from the website
set input_file "$dir_name/input.txt"
if test ! -f $input_file
    echo "Creating $input_file"
    set url "https://adventofcode.com/2022/day/$today/input"
    curl "$url" -X GET -H "Cookie: session=$cookie" >$input_file
end

# Create empty test file
set test_file "$dir_name/test.txt"
if test ! -f $test_file
    echo "Creating $test_file"
    echo "FIXME: Add the actual input for today" >$test_file
end

# Commit changes performed in this script
git add utils/all.ml utils/dune $dir_name
git commit -m "Added template for day $today" --author="Sunrise Script <ambre+bot@tarides.com>"
