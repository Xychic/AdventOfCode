import sys
import collections
import itertools
import numpy as np

passports = []

test = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

passp = ""
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
# for line in test.splitlines():
    if len(line) == 0:
        passports.append(passp)
        passp = ""
    else:
        passp += line.replace("\n"," ")
passports.append(passp)

needed = ["byr", "iyr", "eyr", "hgt","hcl","ecl","pid"]
ans = 0

for p in passports:
    for n in needed:
        if n not in p:
            # print(p, n)
            # input()
            break
    else:
        # print(p)
        ans += 1

print(ans)

