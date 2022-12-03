
with open('input.txt') as f:
    sums = []
    sum = 0
    for line in f:
        if line.isspace():
            sums.append(sum)
            sum = 0
        else:
            sum += int(line)
    sums.append(sum)
    sums.sort(reverse=True)
    print(sums[0])
    print(sums[0]+sums[1]+sums[2])
                
        
