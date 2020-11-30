def fuel(mass):
    return (mass//3) - 2


total = 0
with open("input.txt","r") as file:
    for line in file:
        total += fuel(int(line))

print(total)
