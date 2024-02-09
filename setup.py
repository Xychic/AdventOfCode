#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python3Packages.requests python3Packages.beautifulsoup4 python3Packages.python-dotenv

import argparse
import sys
import os
import requests
import webbrowser
from subprocess import call
from datetime import datetime
from collections import defaultdict
from bs4 import BeautifulSoup
from dotenv import load_dotenv

load_dotenv()

# Getting the current date for defaults
CURRENT_YEAR = datetime.now().year
CURRENT_DAY = datetime.now().day
CURRENT_PATH = sys.path[0]

SESSION = os.environ.get("SESSION")


# Create the argpaser
parser = argparse.ArgumentParser()
parser.add_argument(
    "-y",
    "--year",
    default=CURRENT_YEAR,
    help="The year of advent of code you want to setup for. (Default is current year)",
)
parser.add_argument(
    "-d",
    "--day",
    default=CURRENT_DAY,
    help="The day of advent of code you want to setup for. (Default is current day)",
)
parser.add_argument(
    "-l",
    "--language",
    default="Rust",
    help="The programming language to create the template file for. (Default is .py)",
)
parser.add_argument(
    "-N", "--NoLink", action="store_false", help="Prevents opening link to current day."
)
args = parser.parse_args()

# Get the title of the days challenge
url = f"https://adventofcode.com/{args.year}/day/{args.day}"
response = requests.get(url)
soup = BeautifulSoup(response.text, "html.parser")
title = (
    str(soup.findAll("h2")[0]).split(": ")[1].split(" ---")[0].replace(" ", "_")
)  # Horrible I know
title = "".join([c for c in title if c.isalpha() or c.isdigit() or c == "_"]).rstrip()
if args.NoLink:
    webbrowser.open_new(url)

# Create the dictionary of language extensions (CBA to add any more atm)
EXTENSION_DICT: dict[str, str] = defaultdict(str)
EXTENSION_DICT["Python"] = ".py"
EXTENSION_DICT["Java"] = ".java"
EXTENSION_DICT["C"] = ".c"
EXTENSION_DICT["C++"] = ".cpp"
EXTENSION_DICT["Haskell"] = ".hs"

# I will probably update this if I decide I want to use any other language
TEMPLATE_DICT: dict[str, str] = defaultdict(str)
TEMPLATE_DICT[
    "Python"
] = """import sys
import time
import collections
import itertools
import numpy as np

startTime = time.time()

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    print(line)

print(f"completed in {time.time() - startTime} seconds")
"""
TEMPLATE_DICT[
    "Java"
] = f"""import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

class {title.replace('_','')} {{

    public static void main(String[] args) {{
        BufferedReader reader;
        try {{
            reader = new BufferedReader(new FileReader("../input.txt"));
            String line = reader.readLine();
            while (line != null) {{
                System.out.println(line);
                line = reader.readLine();
            }}
            reader.close();
        }} catch (IOException e) {{
            e.printStackTrace();
        }}
    }}
}}
"""
TEMPLATE_DICT[
    "C"
] = f"""#include <stdio.h>
#include <stdlib.h>

int main(void) {{
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    size_t read;

    fp = fopen("../input.txt", "r");
    if (fp == NULL) {{
        exit(EXIT_FAILURE);
    }}

    while ((read = getline(&line, &len, fp)) != -1) {{
        printf("%s", line);
    }}

    fclose(fp);
    if (line) {{
        free(line);
    }}
    exit(EXIT_SUCCESS);
}}
"""
TEMPLATE_DICT[
    "Haskell"
] = """import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\\n" ++ printArray xs

subsets:: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

replace:: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b 
    | x == a = b : replace xs a b
    | otherwise = x : replace xs a b

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents

    hClose handle
"""

TEMPLATE_DICT[
    "Py1"
] = """\
# type: ignore
print(
    "\\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    input
                ),
                "Part 2: {}".format(
                    "TODO"
                )
            ]
        )(
            open("../input.txt").read().strip().splitlines()
        )
    )
)
"""

TEMPLATE_DICT[
    "Nix"
] = """\
let
  pkgs = import <nixpkgs> {};
in rec {
  inherit (pkgs) lib;

  parse = input: (lib.lists.init (lib.strings.splitString "\\n" input));
  parsed = parse (builtins.readFile ../input.txt);
  
  part1' = input: input;
  part1 = input: (
    lib.fix (
      self: {
        test_result = part1' (parse TEST_1_INPUT);
        result = if self.test_result == TEST_1_ANSWER then part1' input else {expected=TEST_1_ANSWER; received=self.test_result;};
      }
    )
  ).result;

  part2' = input: input;
  part2 = input: (
    lib.fix (
      self: {
        test_result = part2' (parse TEST_2_INPUT);
        result = if self.test_result == TEST_2_ANSWER then part2' input else {expected=TEST_2_ANSWER; received=self.test_result;};
      }
    )
  ).result;

  TEST_1_INPUT = "";
  TEST_1_ANSWER = 0;
  TEST_2_INPUT = TEST_1_INPUT;
  TEST_2_ANSWER = 0;


  result = {part1 = part1 parsed; part2 = part2 parsed;};
}
"""

TEMPLATE_DICT["Go"] = """\
package main

import (
	"errors"
	"fmt"
	"log"
	"os"
	"strings"
	"time"
)

type input = string

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
    fmt.Printf("Parsed input in %v\\n", time.Now().Sub(start))

    start = time.Now()
    part1, err := Part1(inpt)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Part 1: %v, took %v\\n", part1, time.Now().Sub(start))

    start = time.Now()
    part2, err := Part2(inpt)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Part 2: %v, took %v\\n", part2, time.Now().Sub(start))

}

func Parse(inpt string) (input, error) {
    return inpt, nil
}

func Part1(inpt input) (int, error) {
    fmt.Printf("%v\\n", inpt)
    return -1, nil
}

func Part2(inpt input) (int, error) {
    return 0, errors.New("Not yet implemented")
    return 0, nil
}

"""

TEMPLATE_DICT["Go_Test"] = """\
package main

import (
    "log"
    "testing"
)

var TEST_1_INPUT = ""
var TEST_1_ANSWER = 0
var TEST_2_INPUT = TEST_1_INPUT
var TEST_2_ANSWER = 0

func TestPart1(t *testing.T) {
    parsed, err := Parse(TEST_1_INPUT)
    if err != nil {
        log.Fatal(err)
    }
    test_ans, err := Part1(parsed)
    if err != nil {
        t.Errorf("Error::Part1: \\"%v\\"", err)
    } else if test_ans != TEST_1_ANSWER {
        t.Errorf("Part1: Expected %v, received %v", TEST_1_ANSWER, test_ans)
    }
}

// func TestPart2(t *testing.T) {
//     parsed, err := Parse(TEST_2_INPUT)
//     if err != nil {
//         log.Fatal(err)
//     }
//     test_ans, err := Part2(parsed)
//     if err != nil {
//         t.Errorf("Error::Part1: \\"%v\\"", err)
//     } else if test_ans != TEST_2_ANSWER {
//         t.Errorf("Part1: Expected %v, received %v", TEST_2_ANSWER, test_ans)
//     }
// }

"""


dirPath = f"{CURRENT_PATH}/{args.year}/Day{int(args.day):02d}/{args.language}"
if args.language == "Py1":
    dirPath = f"{CURRENT_PATH}/{args.year}/Day{int(args.day):02d}/Python"

# Create the full path for the day
try:
    os.makedirs(dirPath)
except FileExistsError:
    pass

# # If there is a cookies file load it, if not ask for the cookie
# if (os.path.isfile(f"{sys.path[0]}/cookies")):
#     cookies = pickle.load(open(f"{sys.path[0]}/cookies", "rb"))
# else:
#     session = input("Please enter session token: ")
#     cookies = {"session" : session}
#     pickle.dump(cookies, open(f"{sys.path[0]}/cookies", "wb"))
cookies = {"session": SESSION}

# Get the days input file
dayInput = requests.get(
    f"https://adventofcode.com/{args.year}/day/{int(args.day)}/input", cookies=cookies
)
open(f"{CURRENT_PATH}/{args.year}/Day{int(args.day):02d}/input.txt", "wb").write(
    dayInput.content
)


# Write the file template
if not os.listdir(dirPath):
    match args.language:
        case "Rust":
            call(
                f"cargo generate --git https://github.com/Xychic/advent_of_code_template -s -f -n {title.lower()} -d year={args.year} -d day={int(args.day):02d}",
                cwd=dirPath,
                shell=True,
            )
            call(f"cargo test --release", cwd=f"{dirPath}/{title.lower()}", shell=True)
            call(f"code {title.lower()}", cwd=dirPath, shell=True)
        case "Py1":
            open(f"{dirPath}/{title.lower()}_oneline.py", "w").write(
                TEMPLATE_DICT["Py1"]
            )
        case "Nix":
            open(f"{dirPath}/{title.lower()}.nix", "w").write(TEMPLATE_DICT["Nix"])
        case "Go":
            os.makedirs(f"{dirPath}/{title.lower()}")
            call(
                f"go mod init {title.lower()}",
                cwd=f"{dirPath}/{title.lower()}",
                shell=True
            )
            open(f"{dirPath}/{title.lower()}/main.go", "w").write(TEMPLATE_DICT["Go"])
            open(f"{dirPath}/{title.lower()}/{title.lower()}_test.go", "w").write(TEMPLATE_DICT["Go_Test"])
            call(f"code {title.lower()}", cwd=dirPath, shell=True)
        case _:
            for i in range(2):
                open(
                    f"{dirPath}/{title.replace('_','')}-Part{i+1}{EXTENSION_DICT[args.language]}",
                    "w",
                ).write(TEMPLATE_DICT[args.language])
