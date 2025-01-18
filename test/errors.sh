#!/usr/bin/env bash

cmd="$1"

function run {
    printf "\033[34m| Running \"\033[35m%s\033[34m\"...\033[0m\n" "$1"
    $1 | head -n "$2"
    printf "\n"
}

run "$cmd -q choice asdf 0 hahaha" 30
run "$cmd -d pin -q public 0 0 3455" 30
run "$cmd -n (2,2,0,0) -q shuffle qwerty 0 dBCW" 30
run "$cmd -q public 0 0 hahaha" 30
run "$cmd -d pin -q public 0 0 asdfgh" 30
run "$cmd pub hasd-3245 234" 30
run "$cmd pub 45 2--3ho4" 30
run "$cmd pub 4--45 2-34" 30
run "$cmd -c [] pub 4-45 2-34" 30
run "$cmd -n (8,7,1,-3) pub 0 0" 30
run "$cmd -n (8,800,1,1) pub 0 0" 30
run "$cmd -q non-existent pub 0 0" 30
run "$cmd --list pub 3asd5 0" 30
run "$cmd --list pub 5 ahfs234^&%jkdv" 30
run "$cmd -f non-existent pub 5 5" 30
run "$cmd" 30
run "$cmd -d non-existent pub 0 0" 30
run "$cmd -n non-existent pub 0 0" 30
run "$cmd -n (5,9,4,) pub 0 0" 30
run "$cmd -c () pub 0 0" 30
run "$cmd -" 30
run "$cmd -rts haha" 30
run "$cmd -p 3" 30
run "$cmd -p 3 +no-color" 30
run "$cmd asd asd asd +no-color" 30
