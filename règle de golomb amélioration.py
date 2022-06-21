def dessinregle(x):
    regle = ""
    while x >= 1:
        if x % 2 == 1:
            regle += "|"
        else:
            regle += "."
        x = x//2
    return regle

def liste(x):
    l = []
    mem = 0
    while x >= 1:
        if x % 2 == 1:
            l.append(mem)
        mem += 1
        x = x//2
    return l

def dist(x,l):
    d = []
    for i in range(len(l)):
        for j in range(i+1,len(l)):
            if l[j] - l[i] in d:
                return False
            d.append(l[j] - l[i])
    d.sort()
    return d


def golomb(ordre):
    found = False
    x = 0
    if ordre != 0:
        x = 2**(int(((ordre-1/4)**(1/2)-1/2)**4-1))+1

    l = [0]
    regle = []
    while not found:
        l = liste(x)
        if len(l) == ordre:
            d = dist(x,l)
            if d != False:
                found = True
                regle.append(x)
                print(dessinregle(regle[0]))
                print(liste(regle[0]))
        x += 2
    if l != []:
        n = l[-1]
        while l[-1] <= n :
            l = liste(x)
            if len(l) == ordre:
                d = dist(x,l)
                if d != False:
                    regle.append(x)
            x += 2
            l = liste(x)
    for i in range(len(regle)):
        print(regle[i], dessinregle(regle[i]))
        print(liste(regle[i]))
        print()