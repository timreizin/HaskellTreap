import pytest
import os, os.path
import filecmp

def test_small():
    tests = os.path.join(os.getcwd(), "tests/small")
    for i in range(0, 100):
        testIn = os.path.join(tests, f"in{i}.txt")
        testOut = os.path.join(tests, f"out{i}.txt")
        os.system(f"cat {testIn} | runhaskell -i{os.path.join(os.getcwd(), '..')} test.hs > out.txt")
        assert filecmp.cmp(testOut, os.path.join(os.getcwd(), "out.txt")), f"Small test #{i} failed"
        os.system(f"rm out.txt")


def test_big():
    tests = os.path.join(os.getcwd(), "tests/big")
    for i in range(0, 100):
        testIn = os.path.join(tests, f"in{i}.txt")
        testOut = os.path.join(tests, f"out{i}.txt")
        os.system(f"cat {testIn} | runhaskell -i{os.path.join(os.getcwd(), '..')} test.hs > out.txt")
        assert filecmp.cmp(testOut, os.path.join(os.getcwd(), "out.txt")), f"Big test #{i} failed"
        os.system(f"rm out.txt")
        print(f"Big test #{i} done")