#!/usr/bin/env bash

cmd="$1"

test_number=1

function compare { # == \"\033[35m%s\033[34m\"...
    printf "\033[34m| Test #%-2s: \033[35m%-45.45s\033[34m -- \033[0m" "$test_number" "$1"
    output="$($1)"
    if [[ "$output" == "$2" ]]; then
        printf "\033[1;32mPASSED\033[0m\n"
        test_number=$((test_number+1))
    else
        printf "\033[1;31mFAILED\033[0m\n"
        printf "    Wanted: \033[33m%s\033[0m\n" "$2"
        printf "    Got:    \033[33m%s\033[0m\n" "$output"
        exit 1
    fi
}

compare "$cmd -d long AAA0-=~!@ 123 123" "@&FjQ*34kzZu1O\$IGncBb%5Hy"
compare "$cmd -d mediumpin qwerty 89 9045" "185074"
compare "$cmd zxc-%-vbn 89-45 9045-4557" "FkPw=p+VyMjdv6XN\$2^3i@QW4"
compare "$cmd -n (4,6,5,5) hahaha 456 123" "&Uo0b\$u1*=FdEVW4O+26"
compare "$cmd -c [(\"^&*#\",3),(\"cfrvgtbhy\",8),(\"AZSXDCFV\",5)] e3r4t5 5000 1000" "cZD^Ffgtrvh&XbA#"
compare "$cmd -q public 735-5 537-5 X+I?3tioB4f\$y=P^6CkN2SYjp" "\"vftyhb135\""
compare "$cmd -q choice vftyhb135 537-5 X+I?3tioB4f\$y=P^6CkN2SYjp" "214504642209375"
compare "$cmd -q shuffle vftyhb135 735-5 X+I?3tioB4f\$y=P^6CkN2SYjp" "44655137246457"
compare "$cmd --list 1asdfgh1 1 dU*wG6zh!^I0CfqB3&MF5jt-W" "4671573659263123921383069281900060449000 0"
compare "$cmd -f /home/ramak/.config/pshash/pshash.conf phone 123-3 123-3" "926584"
