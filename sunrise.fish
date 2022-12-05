#!/bin/fish

########################################################################
# The purpose of this script is to automate everything needed to move
# from one day to the next. As there are a few different tasks it's easy
# to forget one.
########################################################################

## Find yesterday's date
set yesterday (rg "nb_days = (\d+)" aoc2022/main.ml -Nor '$1')
set today (math $yesterday + 1)
echo "Today's date is the $today"

if test $today -gt 25
    echo "Nothing new for today"
    exit 1
end

## Updating all the files to reference the new day

# aoc2022/main.ml
echo "Updating aoc2022/main.ml"
sed -i "s/\(nb_days = \)[0-9]\+/\1$today/" aoc2022/main.ml
sed -i "s|(\* \(Day$today.day;\) \*)|\1|" aoc2022/main.ml

# aoc2022/dune
echo "Updating aoc2022/dune"
sed -i "s/\(day$yesterday\))/\1 day$today)/" aoc2022/dune

# test/main.ml

echo "Updating test/main.ml"
sed -i "s/\($yesterday.day ]);\)/\1\n    (\"Day $today\", [ Test$today.day ]);/" test/main.ml

# test/dune
echo "Updating test/dune"
sed -i "s/\(day$yesterday\))/\1 day$today)/" test/dune


## Create the new files

# Create directories
set dir_name "./day$today"
if test ! -d "$dir_name"
    echo "Creating directory $dir_name"
    mkdir -p "$dir_name"
end

# Create placeholder ocaml file
echo "Creating $dir_name/day$today.ml"
echo "let day contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  List.hd lines
" >$dir_name/day$today.ml

# Create test file
echo "Creating test/test$today.ml"
echo "let day =
  let open Day$today in
  Alcotest.test_case \"Test puzzle input\" `Quick @@ fun () ->
  let contents = Utils.get_test $today in
  let result = day contents in
  let expected = \"TODO: add actual puzzle input\" in
  Alcotest.(check string) \"puzzle input should be solved!\" expected result;
  ()" >test/test$today.ml

# Create dune file
echo "Creating $dir_name/dune"
echo "(library
 (name day$today)
 (libraries utils))" >$dir_name/dune

# Create empty test file
set test_file "$dir_name/test.txt"
if test ! -f $test_file
    echo "Creating $test_file"
    touch $test_file
end

# Get the needed session cookie
set cookie $AOC2022_SESSION
if test -z "$cookie"
    echo "Session cookie not found!"
    exit 1
end

# Fetch input from the website
set file_name "$dir_name/input.txt"
if test ! -f $file_name
    echo "Creating $file_name"
    set url "https://adventofcode.com/2022/day/$today/input"
    curl "$url" -X GET -H "Cookie: session=$cookie" >$file_name
end

# Format files just in case
dune build @fmt --auto-promote
