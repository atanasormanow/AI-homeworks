import matplotlib.pyplot as plt

def parseCoord(str):
    x,y = [float(x) for x in str.split('\t')]
    return (x,y)

def parseColoredCoord(str):
    x,y,color = str.split(',')
    return (float(x),float(y), color.replace('\n', ''))

with open('static/normal.txt') as f:
    normalPoints = [parseCoord(l) for l in f.readlines()]

with open('out/output.txt') as f:
    clusteredPoints = [parseColoredCoord(l) for l in f.readlines()]

def renderColored(points):
    for x,y,c in points:
        plt.plot(x, y, color=c, marker='o')
    plt.show()

def render(points):
    for x,y in points:
        plt.plot(x, y, color='black', marker='o')
    plt.show()

renderColored(clusteredPoints);
