class IntComp():

    def __init__(self, code, input):
        self.code = code.copy()
        self.overflow = {}
        self.input = input
        self.i = 0
        self.rBase = 0

    def __getitem__(self, pos):
        if pos < len(self.code):
            return self.code[pos]
        else:
            if pos not in self.overflow:
                self.overflow[pos] = 0
            return self.overflow[pos]

    
    def __setitem__(self, pos, item):
        if pos < len(self.code):
            self.code[pos] = item
        else:
            self.overflow[pos] = item
    
    def index(self, pos):
        addressMode = (self[self.i] // (10 * 10**pos)) % 10
        if addressMode == 0:
            return self[self.i+pos]
        elif addressMode == 1:
            return self.i + pos
        elif addressMode == 2:
            return self[self.i+pos] + self.rBase
    
    def val(self, pos):
        return self[self.index(pos)]

    def run(self, timeout=None):
        t = 0
        while (not timeout) or (t<timeout):
            op = self[self.i] % 100
            if op == 1:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self[p3] = p1 + p2
                self.i += 4
            elif op == 2:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self[p3] = p1 * p2
                self.i += 4
            elif op == 3:
                p1 = self.index(1)
                self[p1] = self.input()
                self.i += 2
            elif op == 4:
                p1 = self.val(1)
                self.i += 2
                return p1
            elif op == 5:
                p1, p2 = self.val(1), self.val(2)
                self.i = p2 if bool(p1) else self.i+3
            elif op == 6:
                p1, p2 = self.val(1), self.val(2)
                self.i = p2 if not bool(p1) else self.i+3
            elif op == 7:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self[p3] = 1 if p1 < p2 else 0
                self.i += 4
            elif op == 8:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self[p3] = 1 if p1 == p2 else 0
                self.i += 4
            elif op == 9:
                p1 = self.val(1)
                self.rBase += p1
                self.i += 2
            elif op == 99:
                return None
            t += 1
        return None

with open ("1.s","r") as file:
    code = [int(i) for i in file.read().split(",")]

from itertools import combinations

items = ["space heater", "semiconductor", "hypercube", "spool of cat6", "sand", "festive hat", "dark matter"]
itemCombo = []
for l in range(1, len(items)):
    itemCombo.extend(list(combinations(items, l)))
operations = ["north","west","north","north","east","east","north"] + ["drop " + item for item in items]

for i in itemCombo:
    operations += ["take " + x for x in i] + ["west"] + ["drop " + x for x in i]

text = []
def getInput():
    if len(text) == 0:
        if len(operations) == 0:
            t = input(": ")
        else:
            t = operations.pop(0)
        if t == "save":
            print(comp.code)
            print(comp.i)
        else:
            text.extend([ord(c) for c in (t + "\n")])
    return text.pop(0)

        

message = ""
comp = IntComp(code, getInput)
# comp.i = 2663
res = 0
while res is not None:
    res = comp.run()
    if res == 10:
        print(message)
        message = ""
    else:
        message += chr(res)

