#!/usr/bin/env bash

cmd="$1"

test_number=1

function compare {
    printf "\033[34m| Test #%02d: \033[35m%-45.45s\033[34m -- \033[0m" "$test_number" "$1"
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

compare "$cmd --version" "The pshash pseudo-hash password manager, version 0.1.17.5"
compare "$cmd zxc-%-vbn 89^45 9045^4557" "FkPw=p+VyMjdv6XN\$2^3i@QW4"
compare "$cmd -k long AAA0-=~!@ 123 123" "@&FjQ*34kzZu1O\$IGncBb%5Hy"
compare "$cmd -k medium ##### 9^900*30*99 125^125" "5y=D06Zw*K\$N%8k@1raX"
compare "$cmd -k short ##### 123^1000 666^1" "H\$!x9*PMZ#z6f4h5"
compare "$cmd -k anlong --pure ##### 123^1000 666^1" "bWZHh3y92TzV84XBx6af0"
compare "$cmd -k anshort ##### 123^1000 666^1" "3xPM47Zz2Hfh"
compare "$cmd -k pin ##### 12*43^10+120 666^1" "6720"
compare "$cmd -k mediumpin qwerty 89 9045" "185074"
compare "$cmd -k longpin tyuio 1000 9000" "30624859"
compare "$cmd -n (4,6,5,5) hahaha 456 123" "&Uo0b\$u1*=FdEVW4O+26"
compare "$cmd -n (26,26,0,0) public 456^345 123^0" "RHdyqxtYoAfnwCPXjGTeprmzbcghFJZMuINsaiSElLvOUBkVDQWK"
compare "$cmd -c [(\"^&*#\",3),(\"cfrvgtbhy\",8),(\"AZSXDCFV\",5)] e3r4t5 5000 1000" "cZD^Ffgtrvh&XbA#"
compare "$cmd -q public 735^5 537^5 X+I?3tioB4f\$y=P^6CkN2SYjp" "\"vftyhb135\""
compare "$cmd -q choice vftyhb135 537^5 X+I?3tioB4f\$y=P^6CkN2SYjp" "214504642209375"
compare "$cmd -q shuffle vftyhb135 735^5 X+I?3tioB4f\$y=P^6CkN2SYjp" "44655137246457"
compare "$cmd --list 1asdfgh1 1 dU*wG6zh!^I0CfqB3&MF5jt-W" "4671573659263123921383069281900060449000 0"
compare "$cmd -f test/pshash.conf google 123 123" "03NbhL7aoSVGRYjt28y19"
compare "$cmd -k anlong -p 66 qwer 90 90" "Lh5b1q92I4VzRriNB87eU"
compare "$cmd -f test/pshash.conf overleaf 876^5 139 -p 4" "3F1SVZD"
$cmd -r 24 -e test/data.txt test/data.enc 2345^45 123^322
compare "$cmd -r 24 -d test/data.enc stdout 2345^45 123^322" "$(cat "test/data.txt")"
compare "$cmd -k anlong puuub uskehaunpicadopebewokabeitagelelcavemuduunba tifijalierfumeterotejoquve" "K4QEmk0hx5uYB3vFb187S"
compare "$cmd -n (24,13,10,7) magiiiic 1234^247+1237*123 ityilereorbafafapijahofemefule" "ecdwLxq@^IayFC2SEb%-nr8Blvz*6#O1mZAhg!5o?ut+psj9\$QiW7X"
compare "$cmd --gen-spell 123^11" "yudayi tikipu higogi suwaca"
compare "$cmd --gen-num hahamuhamuha 6969" "$(printf "4473\nhabu")"
