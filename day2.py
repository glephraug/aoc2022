
with open('input2.txt') as f:
    score = 0
    for line in f:
        s = 0
        them, us = line.split()
        if us == "X":
            score += 1
            s += 0
        elif us == "Y":
            score += 2
            s += 1
        elif us == "Z":
            score += 3
            s += 2
        if them == "A":
            s += 1
        elif them == "B":
            s += 0
        elif them == "C":
            s += 2
        score += (s%3)*3
        #print(them + " " + us + " " + str(score))
    print("part 1 score " + str(score))

with open('input2.txt') as f:
    score = 0
    for line in f:
        s = 0
        them, us = line.split()
        if us == "X":
            score += 0
            s += 0
        elif us == "Y":
            score += 3
            s += 1
        elif us == "Z":
            score += 6
            s += 2
        if them == "A":
            s += 2
        elif them == "B":
            s += 0
        elif them == "C":
            s += 1
        score += (s%3)+1
        #print(them + " " + us + " " + str(score))
    print("part 2 score " + str(score))
