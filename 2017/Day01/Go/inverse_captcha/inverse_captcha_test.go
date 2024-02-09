package main

import (
    "testing"
    "log"
)

var TEST_1_INPUT = "91212129";
var TEST_1_ANSWER = 9;
var TEST_2_INPUT = TEST_1_INPUT;
var TEST_2_ANSWER = 6;

func TestPart1(t *testing.T) {
    parsed, err := Parse(TEST_1_INPUT)
    if err != nil {
        log.Fatal(err)
    }
    test_ans, err := Part1(parsed)
    if err != nil {
        t.Errorf("Error::Part1: \"%v\"", err)
    } else if test_ans != TEST_1_ANSWER {
        t.Errorf("Part1: Expected %v, received %v", TEST_1_ANSWER, test_ans)
    }
}

func TestPart2(t *testing.T) {
    parsed, err := Parse(TEST_2_INPUT)
    if err != nil {
        log.Fatal(err)
    }
    test_ans, err := Part2(parsed)
    if err != nil {
        t.Errorf("Error::Part1: \"%v\"", err)
    } else if test_ans != TEST_2_ANSWER {
        t.Errorf("Part1: Expected %v, received %v", TEST_2_ANSWER, test_ans)
    }
}

