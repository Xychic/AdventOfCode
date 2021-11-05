class point():

    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def distance(self, otherPoint):
        return abs(self.x - otherPoint.x) + abs(self.y - otherPoint.y)

    def __str__(self):
        return "({0},{1})".format(self.x,self.y)
    
    def __repr__(self):
        return self.__str__()
    
    def __eq__(self, other):
        return (self.x == other.x) & (self.y == other.y)
    
    def move(self, instruction):
        if instruction[0] == "R":
            return point(self.x + int(instruction[1:]), self.y)
        elif instruction[0] == "L":
            return point(self.x - int(instruction[1:]), self.y)
        elif instruction[0] == "U":
            return point(self.x, self.y + int(instruction[1:]))
        elif instruction[0] == "D":
            return point(self.x, self.y - int(instruction[1:]))
    
    def moveAll(self, instruction):
        if instruction[0] == "R":
            return [point(self.x + i + 1, self.y) for i in range(int(instruction[1:]))]
        elif instruction[0] == "L":
            return [point(self.x - i -1, self.y) for i in range(int(instruction[1:]))]
        elif instruction[0] == "U":
            return [point(self.x, self.y + i + 1) for i in range(int(instruction[1:]))]
        elif instruction[0] == "D":
            return [point(self.x, self.y - i + 1) for i in range(int(instruction[1:]))]

class wire():
    def __init__(self, instructions):
        self.points = [point(0,0)]
        self.pointsAll = [point(0,0)]
        for op in instructions:
            p = self.points[-1]
            self.points.append(p.move(op))
            self.pointsAll.extend(p.moveAll(op))

    def __str__(self):
        return str(self.points)

        
    
def intersection(p1, p2, p3, p4):
    if (min(p1.x,p2.x) <= p3.x <= max(p1.x, p2.x) and min(p3.y, p4.y) <= p1.y <= max(p3.y, p4.y)):
        return True, point(p3.x, p1.y)
    elif (min(p3.x,p4.x) <= p1.x <= max(p3.x, p4.x) and min(p1.y, p2.y) <= p3.y <= max(p1.y, p2.y)):
        return True, point(p1.x, p3.y)
    else:
        return False, None

wires = []
with open("input.txt","r") as f:
    for line in f:
        wires.append(wire(line.split(",")))

crossOver = []

for i in range(len(wires[0].points)-1):
    for j in range(len(wires[1].points)-1):
        r, p = intersection(wires[0].points[i],wires[0].points[i+1],wires[1].points[j],wires[1].points[j+1])
        if r:
            crossOver.append(p)

distances = [wires[0].pointsAll.index(c) + wires[1].pointsAll.index(c) for c in crossOver[1:]]
print(min(distances))