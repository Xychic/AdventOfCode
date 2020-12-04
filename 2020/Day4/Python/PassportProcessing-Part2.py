import sys
import collections
import itertools
import numpy as np
import re

passports = []

passp = ""
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    if len(line) == 0:
        passports.append(passp)
        passp = ""
    else:
        passp += " " + line.replace("\n"," ")
passports.append(passp)    

needed = ["byr", "iyr", "eyr", "hgt","hcl","ecl","pid"]

def validate(cat, val):
    if cat == "byr":
        return 1920 <= int(val) <= 2002
    elif cat == "iyr":
        return 2010 <= int(val) <= 2020
    elif cat == "eyr":
        return 2020 <= int(val) <= 2030
    elif cat == "hgt":
        unit = val[-2:]
        height = val[:-2]
        if unit == "cm":
            return 150 <= int(height) <= 193
        elif unit == "in":
            return 59 <= int(height) <= 76
    elif cat == "hcl":
        return bool(re.match(r"^#[0-9a-f]{6}$", val))
    elif cat == "ecl":
        return val in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    elif cat == "pid":
        return bool(re.match(r"^[0-9]{9}$", val))
    elif cat == "cid":
        return True
    return False
ans = 0

passSplit = []

for p in passports:
    for n in needed:
        if n not in p:
            break
    else:
        ps = p.split()
        for i, pss in enumerate(ps):
            ps[i] = pss.split(":")
        passSplit.append(ps)

print(len(passSplit))
   
for p in passSplit:
    for cat, val in p:
        if not validate(cat, val):
            break
    else:
        ans += 1

print(ans)