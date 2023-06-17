#!/usr/bin/env python3

import os, os.path
import random

dir = os.getcwd()

os.system("g++-12 -std=c++20 -o makeTest makeTest.cpp")

for f in os.listdir(os.path.join(dir, "tests/small")):
    os.remove(os.path.join(dir, "tests/small", f))

for f in os.listdir(os.path.join(dir, "tests/big")):
    os.remove(os.path.join(dir, "tests/big", f))

for i in range(0, 100):
    os.system(f"./makeTest {i} {random.randint(-1000000000, 1000000000)} {-10000} {10000} {1000} {1000}")
    os.system(f"mv in{i}.txt tests/small/in{i}.txt")
    os.system(f"mv out{i}.txt tests/small/out{i}.txt")
    print(f"Small test {i} ready")

for i in range(0, 100):
    os.system(f"./makeTest {i} {random.randint(-1000000000, 1000000000)} {-10000} {10000} {100000} {100000} 16789ABCDFGHIJ")
    os.system(f"mv in{i}.txt tests/big/in{i}.txt")
    os.system(f"mv out{i}.txt tests/big/out{i}.txt")
    print(f"Big test {i} ready")

os.remove("makeTest")