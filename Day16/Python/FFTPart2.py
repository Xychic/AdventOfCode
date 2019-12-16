import numpy as np

with open("input.txt") as f:
    signal = list(f.read().strip())

shift = int("".join(signal[0:7]))
signal = np.array([int(x) for x in signal]*10000)

signal = signal[shift:]

for i in range(100):
    values = np.cumsum(signal[::-1]) % 10
    signal = values[::-1]
    

for i in range(0,8):
    print(signal[i], end="")
print()
