def stack(deck, null):
    return deck[::-1]

def cut(deck, amount):
        return deck[amount:] + deck[:amount]
        
def deal(deck, amount):
    l = len(deck)
    res = [None for _ in range(l)]
    for i in range(l):
        res[(i*amount)%l] = deck[i]
    return res

ops = {
    "cut" : cut,
    "increment" : deal,
    "stack" : stack
}

operations = []

for line in open("../input.txt"):
    l = line.strip().split()
    if "cut" in l:
        operations.append(("cut", int(l[-1])))
    elif "increment" in l:
        operations.append(("increment", int(l[-1])))
    elif "stack" in l:
        operations.append(("stack", None))

cards = [i for i in range(10007)]
for o, num in operations:
    cards = ops[o](cards, num)

print(cards.index(2019))
