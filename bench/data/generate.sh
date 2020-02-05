#!/bin/bash
for ((i=1;i<=500000;i++)); \
do \
    echo -n "x " >> ./bench-test-dense.txt; \
done;
