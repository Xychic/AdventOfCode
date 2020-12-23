import sys
import collections
import itertools
import numpy as np

class Node:
    def __init__(self, linkedList, val, prevNode=None, nextNode=None):
        self.parent = linkedList
        self.val = val
        self.prev = prevNode
        self.next = nextNode

class DoubleLinkedList:
    def __init__(self):
        self.nodeDict = {}

    def append(self, after, val):
        if after not in self.nodeDict:
            newNode = Node(self, val)
            newNode.next = newNode
            newNode.prev = newNode
        else:
            previous = self.nodeDict[after]
            newNode = Node(self, val, previous, previous.next)
            previous.next.prev = newNode
            previous.next = newNode
        self.nodeDict[val] = newNode
    
    def remove(self, val):
        toRemove = self.nodeDict[val]
        toRemove.prev.next = toRemove.next
        toRemove.next.prev = toRemove.prev
        del self.nodeDict[val]
    
    def getNode(self, val):
        return self.nodeDict[val]
    
    def contains(self, val):
        return val in self.nodeDict

    def toList(self):
        node = list(self.nodeDict.values())[0]
        result = [node.val]
        node = node.next
        while node.val != result[0]:
            result.append(node.val)
            node = node.next
        return result
    
    def __str__(self):
        return str(self.toList())


inp = "389125467"
inp = open(f"{sys.path[0]}/../input.txt").read().strip()
values = [int(a) for a in inp]
maxVal = max(values)
while len(values) < int(1e6):
    values.append(maxVal+1)
    maxVal += 1

data = DoubleLinkedList()
prev = None
for val in values:
    data.append(prev, val)
    prev = val

currentNode = data.getNode(values[0])
for move in range(int(1e7)):
    currentVal = currentNode.val

    pick = []
    pickNode = currentNode
    for _ in range(3):
        pickNode = pickNode.next
        pick.append(pickNode.val)
        data.remove(pickNode.val)
    
    destination = currentVal -1
    while not data.contains(destination):
        if destination == 0:
            destination = max(data.toList())
        else:
            destination -= 1
    
    insert = data.getNode(destination)
    for p in pick:
        data.append(insert.val, p)
        insert = data.getNode(p)
    currentNode = currentNode.next

print(data.getNode(1).next.val * data.getNode(1).next.next.val)