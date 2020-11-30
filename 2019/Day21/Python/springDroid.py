class IntComp():

    def __init__(self, memory):
        self.code = memory
        self.overflow = {}
        self.i = 0
        self.rBase = 0
            
    def __getitem__(self, i):
        if i < len(self.code):
            return self.code[i]
        else:
            if i not in self.overflow:
                self.overflow[i] = 0
            return self.overflow[i]

    
    def __setitem__(self, i, item):
        if i < len(self.code):
            self.code[i] = item
        else:
            self.overflow[i] = item
    
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
    
    def run(self, inputs=[]):
        out = []
        while True:
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
                self[p1] = inputs.pop(0)
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
                self[p3] = 1 if p1<p2 else 0
                self.i += 4
            elif op == 8:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self[p3] = 1 if p1==p2 else 0
                self.i += 4
            elif op == 9:
                p1 = self.val(1)
                self.rBase += p1
                self.i += 2
            elif op == 99:
                return None

with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

comp = IntComp(code)

message = ""

program = """OR A J
AND B J
AND C J
NOT J J
AND D J
WALK
"""
program = [ord(c) for c in program]
res = comp.run(program)
while res is not None:
    if res == 10:
        print(message)
        message = ""
    elif res > 255:
        print(res)
    else:
        message += chr(res)
    res = comp.run(program)