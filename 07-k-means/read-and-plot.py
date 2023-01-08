import matplotlib.pyplot as plt

def parseNormalCoord(str):
    x,y = [float(x) for x in str.split('\t')]
    return (x,y)

def parseUnbalancedCoord(str):
    x,y = [int(x) for x in str.split(' ')]
    return (x,y)

def parseColoredCoord(str):
    x,y,color = str.split(',')
    return (float(x),float(y), color.replace('\n', ''))

with open('static/unbalance.txt') as f:
    unbalancedPoints = [parseUnbalancedCoord(l) for l in f.readlines()]

with open('static/unbalance.txt') as f:
    normalPoints = [parseUnbalancedCoord(l) for l in f.readlines()]

with open('out/output.txt') as f:
    clusteredNormalPoints = [parseColoredCoord(l) for l in f.readlines()]

def renderColored(points):
    for x,y,c in clusteredNormalPoints:
        plt.plot(x, y, color=c, marker='o')
    plt.show()

def render(points):
    for x,y in normalPoints:
        plt.plot(x, y, color='black', marker='o')
    plt.show()

renderColored(clusteredNormalPoints);
#  render(unbalancedPoints)
