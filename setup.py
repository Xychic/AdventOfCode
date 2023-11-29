#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python3Packages.requests python3Packages.beautifulsoup4 python3Packages.python-dotenv

import argparse
import sys
import os
import requests
import pickle
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
EXTENSION_DICT = defaultdict(str)
EXTENSION_DICT["Python"] = ".py"
EXTENSION_DICT["Java"] = ".java"
EXTENSION_DICT["C"] = ".c"
EXTENSION_DICT["C++"] = ".cpp"
EXTENSION_DICT["Haskell"] = ".hs"

# I will probably update this if I decide I want to use any other language
TEMPLATE_DICT = defaultdict(str)
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
        case _:
            for i in range(2):
                open(
                    f"{dirPath}/{title.replace('_','')}-Part{i+1}{EXTENSION_DICT[args.language]}",
                    "w",
                ).write(TEMPLATE_DICT[args.language])
