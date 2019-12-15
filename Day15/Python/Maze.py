import random as rand

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
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self.code[p3] = p1 + p2
                self.i += 4
            elif op == 2:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self.code[p3] = p1 * p2
                self.i += 4
            elif op == 3:
                p1 = self.index(1)
                self.code[p1] = inputs.pop(0)
                self.i += 2
            elif op == 4:
                p1 = self.val(1)
                self.i += 2
                return p1
            elif op == 5:
                p1, p2 = self.val(1), self.val(2)
                self.i = p2 if p1 == 1 else self.i+3
            elif op == 6:
                p1, p2 = self.val(1), self.val(2)
                self.i = p2 if p1 == 0 else self.i+3
            elif op == 7:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self.code[p3] = 1 if p1<p2 else 0
                self.i += 4
            elif op == 8:
                p1, p2, p3 = self.val(1), self.val(2), self.index(3)
                self.code[p3] = 1 if p1==p2 else 0
                self.i += 4
            elif op == 9:
                p1 = self.val(1)
                self.rBase += p1
                self.i += 2

with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]
code += [0] * 9999999

best = float("INF")
walls = [["  " for _ in range(50)] for _ in range(50)]
inverse = [2,1,4,3]
move = [
    (0,1),
    (0,-1),
    (-1,0),
    (1,0)
]

count = 100
for test in range(count):
    directions = [[[1,2,3,4] for _ in range(50)] for _ in range(50)]
    stack = []
    pos = [25,25]
    walls[25][25] = "x "

    droid = IntComp(code.copy())
    dir = rand.choice(directions[pos[1]][pos[0]])
    res = droid.run([dir])

    while True:
        directions[pos[1]][pos[0]].remove(dir)

        if res == 1:
            stack.append(dir)
            pos[0] += move[dir-1][0]
            pos[1] += move[dir-1][1]
        elif res == 2:
            walls[pos[1] + move[dir-1][1]][pos[0] + move[dir-1][0]] = "O"
            break
        else:
            walls[pos[1] + move[dir-1][1]][pos[0] + move[dir-1][0]] = "##"

        while len(directions[pos[1]][pos[0]]) == 0:
            dir = stack.pop()
            pos[0] -= move[dir-1][0]
            pos[1] -= move[dir-1][1]
            droid.run([inverse[dir-1]]) 
            
        dir = rand.choice(directions[pos[1]][pos[0]])
        res = droid.run([dir])
    best = min(best, len(stack)+1)
    print("Test {0}/{1}".format(test,count))

for row in walls:
    print("".join(row))

print(best)