class IntComp():

    def __init__(self, code):
        self.code = code
        self.i = 0
        self.rBase = 0

    
    def AM(self, pos):
        return (self.code[self.i] // (10 * 10**pos)) % 10


    def index(self, pos):
        addressMode = (self.code[self.i] // (10 * 10**pos)) % 10
        if addressMode == 0:
            return self.code[self.i+pos]
        elif addressMode == 1:
            return self.i + pos
        elif addressMode == 2:
            return self.code[self.i+pos] + self.rBase

    
    def val(self,pos):
        return self.code[self.index(pos)]
        

    def run(self, inputs=[]):
        while True:
            self.i %= len(self.code)
            op = self.code[self.i] % 100

            if op == 99:
                return None
            elif op == 1:
                p1 = self.val(1)
                p2 = self.val(2)
                p3 = self.index(3)
                self.code[p3] = p1 + p2
                self.i += 4
            elif op == 2:
                p1 = self.val(1)
                p2 = self.val(2) 
                p3 = self.index(3)
                self.code[p3] = p1 * p2
                self.i += 4
            elif op == 3:
                p1 = self.index(1)
                self.code[p3] = inputs.pop(0)
                self.i += 2
            elif op == 4:
                p1 = self.val(1)
                self.i += 2
                return p1
            elif op == 5:
                p1 = self.val(1)
                p2 = self.val(2)
                self.i = p2 if p1 == 1 else self.i+3
            elif op == 6:
                p1 = self.val(1)
                p2 = self.val(2)
                self.i = p2 if p1 == 0 else self.i+3
            elif op == 7:
                p1 = self.val(1)
                p2 = self.val(2)
                p3 = self.index(3)
                self.code[p3] = 1 if p1<p2 else 0
                self.i += 4
            elif op == 8:
                p1 = self.val(1)
                p2 = self.val(2)
                p3 = self.index(3)
                self.code[p3] = 1 if p1==p2 else 0
                self.i += 4
            elif op == 9:
                p1 = self.val(1)
                self.rBase += p1
                self.i += 2

        
with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]
code += [0] * 9999999

P = IntComp(code)
res = P.run([2])
while res != None:
    print(res)
    res = P.run()