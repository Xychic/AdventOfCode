package main

import (
    "fmt"
    "log"
    "os"
    // "errors"
    "time"
    "strings"
    "strconv"
)

type input = []int;

func main() {
    content, err := os.ReadFile("../../input.txt")
    if err != nil {
        log.Fatal(err)
    }
    start := time.Now()
    inpt, err := Parse(strings.TrimSpace(string(content)))
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Parsed input in %v\n", time.Now().Sub(start))

    start = time.Now()
    part1, err := Part1(inpt)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Part 1: %v, took %v\n", part1, time.Now().Sub(start))

    start = time.Now()
    part2, err := Part2(inpt)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Part 2: %v, took %v\n", part2, time.Now().Sub(start))
}

func Parse(inpt string) (input, error) {
    split := strings.SplitAfter(inpt, "")
    parsed := make([]int, len(split))
    var err error;
    for i, v := range split {
        if parsed[i], err = strconv.Atoi(v); err != nil {
            log.Fatal(err)
        }
    }
    return parsed, nil
}

func Part1(inpt input) (int, error) {
    ans := 0;
    for i:=0; i<len(inpt); i++ {
        if inpt[i] == inpt[(i+1)%len(inpt)] {
            ans += inpt[i]
        }
    }
    return ans, nil
}

func Part2(inpt input) (int, error) {
    ans := 0;
    for i:=0; i<len(inpt); i++ {
        if inpt[i] == inpt[(i+len(inpt)/2)%len(inpt)] {
            ans += inpt[i]
        }
    }
    return ans, nil
}
