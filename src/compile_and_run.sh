#!/bin/bash

# simple script to compile and run a tony program

filename=../testcases/hello.tony

./tc $filename  # if successful, "ir.ll" is created

if [ $? -ne 0 ]; then
  exit
fi

llc-14 ir.ll  # compile llvm IR to assembly, "ir.s" is created

clang -c builtins.c

clang -no-pie ir.s builtins.o

./a.out

rm ir.ll ir.s builtins.o a.out
