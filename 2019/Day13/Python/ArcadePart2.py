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

# for i in range(len(code)-2):
#     if code[i] == 0 and code[i+1] == 3 and code[i+2] == 0:
#         toChange = i + 1
#         break

# for r in range(toChange - 17, toChange + 18):
#     code[r] = 1

code += [0] * 9999999
code[0] = 2

DISPLAY = True

comp = IntComp(code)
blocks = float("INF")
ball = paddle = (0,0)
move = 0
for _ in range(259):
    comp.i = 0
    board = [[" " for x in range(35)] for y in range(23)]

    while True:
        x, y, id = comp.run([move]), comp.run([move]), comp.run([move])

        if x is None or y is None or id is None or x == y == id == 0:
            break
        elif x >= 0:
            if id == 1:
                board[y][x] = "\033[1;31;40m#"
            elif id == 2:
                board[y][x] = "\033[1;32;40mX"
            elif id == 3:
                board[y][x] = "\033[1;33;40m-"
                oldX, oldY = paddle
                board[oldY][oldX] = " "
                paddle = (x, y)
            elif id == 4:
                board[y][x] = "\033[1;34;40mO"
                oldX, oldY = ball
                board[oldY][oldX] = " "
                ball = (x, y)
        elif x == -1:
            score = id
        
        if ball[0] < paddle[0]:
            move = -1
        elif ball[0] > paddle[0]:
            move = 1
        else:
            move = 0
    if DISPLAY:
        text = "[Score: {}]".format(score)
        text = "#" * ((35-len(text))//2) + text
        text += "#" * (35-len(text))
        print("\n"+text)
        for i in range(1, 23):
            print("".join(board[i]))

print("Final Score:",score)
