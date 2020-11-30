def fuel(mass):
    res = (mass//3) - 2
    if res > 0:
        return res + fuel(res)
    else:
        return 0

total = 0
with open("input.txt","r") as file:
    for line in file:
        total += fuel(int(line))

print(total)
