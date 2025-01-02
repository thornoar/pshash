#!/usr/bin/env bash

cmd="$1"

function run {
    printf "\033[34m| Running \"\033[35m%s\033[34m\"...\033[0m\n" "$1"
    $1 | head -n "$2"
}

run "$cmd --version" 1
run "$cmd pub1 pub2 0 0" 30
run "$cmd pub hasd-3245 234" 30
run "$cmd pub 45 2--3ho4" 30
run "$cmd -d non-existent pub 0 0" 30
run "$cmd -n non-existent pub 0 0" 30
run "$cmd -n (8,800,1,1) pub 0 0" 30
run "$cmd -n (8,7,1,-3) pub 0 0" 30
run "$cmd -n (5,9,4,) pub 0 0" 30
run "$cmd -c () pub 0 0" 30
run "$cmd -c [(\"nd45\",56)] pub 0 0" 30
