import sys
import collections
import itertools
import numpy as np
import string

ruleDict = {}
seen = {}
rules, messages = open(f"{sys.path[0]}/../input.txt").read().split("\n\n")
rules = rules.replace("\"", "").split("\n")
for r in rules:
    rNum, rData = r.split(": ")
    ruleDict[rNum] = rData.split(" | ")
messages = messages.split("\n")

def generateLanguage(ruleNum):
    if ruleNum in seen:
        return seen[ruleNum]
    rule = ruleDict[ruleNum]
    if rule[0] in string.ascii_lowercase:
        seen[ruleNum] = [rule[0]]
        return [rule[0]]
    else:
        lang = []
        for r in rule:
            s = [""]
            for subr in r.split():
                s = ["".join(s) for s in (itertools.product(s, generateLanguage(subr)))]
            lang += s
        seen[ruleNum] = lang
        return lang
                
lang = set(generateLanguage("0"))
print(len(lang.intersection(messages)))