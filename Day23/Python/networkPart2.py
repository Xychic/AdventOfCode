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

packets = {}
def getInput(i):
    if packets[i] != []:
        return packets[i].pop(0)
    return -1
    
with open ("../input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

comps = []
for i in range(50):
  comps.append(IntComp(code, lambda: getInput(i)))
  packets[i] = [i]

NAT = None
oldNat = None
counter = 0
loop = True

while loop:
    if counter > 400 and NAT is not None:
        packets[0].extend(NAT)
        counter = 0
        print(NAT, oldNat)
        if oldNat is not None and NAT[1] == oldNat:
            print(oldNat)
            loop = False
            break
        oldNat = NAT[1]
    for i in range(len(comps)):
        addr = comps[i].run(timeout=10)
        if addr is None:
            continue
        x, y = comps[i].run(), comps[i].run()
        if addr == 255:
            NAT = (x, y)
        else:
            packets[addr].extend([x,y])
            counter = 0
    counter += 1
