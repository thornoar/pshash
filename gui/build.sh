#!/usr/bin/env bash

g++ -g -O ./main.cpp ./inputs.cpp ./algorithm.c ./mini-gmp.c -o main $(wx-config --cxxflags --libs)

# gcc -g -O ./algorithm.c ./mini-gmp.c -o ./algorithm
# ghc ./Algorithm.hs -o ./Algorithm
