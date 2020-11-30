import argparse, sys, os, requests, pickle
from datetime import datetime
from collections import defaultdict
from bs4 import BeautifulSoup

# Getting the current date for defaults
CURRENT_YEAR = datetime.now().year
CURRENT_DAY = datetime.now().day
CURRENT_PATH = sys.path[0]

# Create the argpaser
parser = argparse.ArgumentParser()
parser.add_argument("-y", "--year", default=CURRENT_YEAR, help="The year of advent of code you want to setup for. (Default is current year)")
parser.add_argument("-d", "--day", default=CURRENT_DAY, help="The day of advent of code you want to setup for. (Default is current day)")
parser.add_argument("-l", "--language", default="Python", help="The programming language to create the template file for. (Default is .py)")
args = parser.parse_args()

# Get the title of the days challenge
response = requests.get(f"https://adventofcode.com/{args.year}/day/{args.day}")
soup = BeautifulSoup(response.text, "html.parser")
title = str(soup.findAll("h2")[0]).split(": ")[1].split(" ---")[0].replace(" ", "_")    # Horrible I know

# Create the dictionary of langugae extensions (CBA to add any more atm)
EXTENSION_DICT = defaultdict(str)
EXTENSION_DICT["Python"] = ".py"
EXTENSION_DICT["Java"] = ".java"
EXTENSION_DICT["C"] = ".c"
EXTENSION_DICT["C++"] = ".cpp"
EXTENSION_DICT["Haskell"] = ".hs"

# I will probably update this if I decide I want to use any other language
TEMPLATE_DICT = defaultdict(str)
TEMPLATE_DICT["Python"] = """import sys, collections

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    print(line)
"""
TEMPLATE_DICT["Java"] = f"""import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

class {title} {{

    public static void main(String[] args) {{
        BufferedReader reader;
        try {{
            reader = new BufferedReader(new FileReader("{CURRENT_PATH}/{args.year}/Day{args.day}/input.txt"));
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
TEMPLATE_DICT["C"] = f"""#include <stdio.h>
#include <stdlib.h>

int main(void) {{
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    size_t read;

    fp = fopen("{CURRENT_PATH}/{args.year}/Day{args.day}/input.txt", "r");
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

# Create the full path for the day
try:    os.makedirs(f"{CURRENT_PATH}/{args.year}/Day{args.day}/{args.language}")
except FileExistsError: pass

# If there is a cookies file load it, if not ask for the cookie
if (os.path.isfile(f"{sys.path[0]}/cookies")):
    cookies = pickle.load(open(f"{sys.path[0]}/cookies", "rb"))
else:
    session = input("Please enter session token: ")
    cookies = {"session" : session}
    pickle.dump(cookies, open(f"{sys.path[0]}/cookies", "wb"))

# Get the days input file
dayInput = requests.get(f"https://adventofcode.com/{args.year}/day/{args.day}/input", cookies=cookies)
open(f"{CURRENT_PATH}/{args.year}/Day{args.day}/input.txt", "wb").write(dayInput.content)

# Write the file template
open(f"{CURRENT_PATH}/{args.year}/Day{args.day}/{args.language}/{title + EXTENSION_DICT[args.language]}", "w").write(TEMPLATE_DICT[args.language])