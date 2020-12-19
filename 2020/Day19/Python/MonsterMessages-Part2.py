import sys
import collections
import itertools
import numpy as np
import string

ruleDict = {}
rules, messages = open(f"{sys.path[0]}/../input.txt").read().split("\n\n")
rules = rules.replace("\"", "").split("\n")
for r in rules:
    rNum, rData = r.split(": ")
    ruleDict[rNum] = rData.split(" | ")

ruleDict["8"] = ["42","42 8"]
ruleDict["11"] = ["42 31", "42 11 31"]
messages = messages.splitlines()

def inLanguage(message, rules):
    if rules == []:
        return len(message) == 0
    subRules = ruleDict[rules[0]]
    if subRules[0] in string.ascii_lowercase:
        return False if (len(message) == 0) else ((message[0] == subRules[0]) and inLanguage(message[1:], rules[1:]))
    else:
        return any([inLanguage(message, r.split() + rules[1:]) for r in subRules])

print(sum([inLanguage(m, ["0"]) for m in messages]))
