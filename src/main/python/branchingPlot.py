import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

# AMS 2021: Visualization of nothing-at-stake branching with local dynamic difficulty leader eligibility.
# Local difficulty adjustments are shown with tracer colors in the branching diagram (left).
# Adversarial pseudo-predictable nonces are shown in the nonce distribution (right).
# The pseudo-predictable nonces are super-imposed along the diagonal of the branching diagram.
# Vertical solid lines represent extensions, many extensions may overlap with random colors to show complexity.
# Automata are represented as branches (parent slot / block number pairs).
# Honest extensions assume 1-alpha resources split among each maximum length branch with distinct parent slots.

# Set seed for reproducibility
# np.random.seed(123)

# Number of Slots
N = 100
# Slots
x = np.arange(N)
# Adversarial nonces
ys = np.random.rand(N)
# Proportion of adversarial stake
alpha = 0.5
# Forging window
gamma = 15
# Snowplow amplitude
fa = 0.5
# Set slope of snowplow
c = 0.0
if gamma>0:
    c = fa/gamma
# Baseline difficulty
fb = 0.05
# Maximum difference between leading block number and viable branches
# branches with a gap greater than branchDepth are cut
branchDepth = 4

# Snowplow curve
def f(d):
    if d < gamma+1:
        return c*d
    else:
        return fb
       
# Staking threshold       
def phi(d,a):
    return 1.0 - (1.0-f(d))**a

# Nonce distribution plot area and colors
area = []
colors = []

# Generates the spectrum of colors for trails in branching diagram
def genSpectrum(g):
    if g>0:
        spec = cm.rainbow(np.linspace(0,1,g))
        spec = np.append(spec,[[0.0, 0.0, 0.0, 1.0]],axis=0)
        return spec
    else:
        return np.array([[1.0,0.0,0.0,1.0],[0.0, 0.0, 0.0, 1.0]])

spectrum = genSpectrum(gamma)

# Tracer points for nothing-at-stake branching
trails = np.zeros((N,N),dtype=int)

fig = plt.figure(figsize=(16,7))
subPlotLeft = fig.add_subplot(121)
subPlotRight = fig.add_subplot(122)

# Empty string with genesis prefix
# rows are branches, columns are parent slot / block number
branches = np.zeros((1, 2), dtype=int)
honestHeads = np.array([[0,1.0-alpha,0]])

i = 0
for y in ys:
    if gamma>0:
        if y > phi(gamma,alpha):
            area.append(1.0)
            colors.append(spectrum[gamma])
            subPlotLeft.scatter(x[i], x[i], s=1.0, c=spectrum[gamma].reshape(1,-1))
        else:
            j = gamma
            while y > phi(gamma-j,alpha):
                j = j - 1
            area.append(2.0*j)
            colors.append(spectrum[gamma-j])
            subPlotLeft.scatter(x[i], x[i], s=j, c=spectrum[gamma-j].reshape(1,-1),alpha=0.5)
    else:
        if y > phi(1,alpha):
            area.append(1.0)
            colors.append(spectrum[1])
            subPlotLeft.scatter(x[i], x[i], s=1.0, c=spectrum[1].reshape(1,-1))
        else:
            area.append(25.0)
            colors.append(spectrum[0])
            subPlotLeft.scatter(x[i], x[i], s=25.0, c=spectrum[0].reshape(1,-1),alpha=0.5)
    newParents = []
    for branch in branches:
        oldParent = branch[0]
        if y < phi(x[i]-branch[0],alpha):
            newParents.append([branch[0],branch[1]])
            branch[0] = x[i]
            branch[1] = branch[1]+1
            subPlotLeft.plot([x[i],x[i]],[oldParent,branch[0]])
        if gamma>0:
            trails[oldParent,int(x[i])] = int(min(x[i]-oldParent,gamma+1))
        else:
            trails[oldParent,int(x[i])] = int(x[i]-oldParent)
    def honestTest():
        for head in honestHeads:
            if np.random.uniform(0.0,1.0) < phi(x[i]-head[0],head[1]):
                newParents.append([x[i],int(head[2]+1)])
                subPlotLeft.plot([x[i],x[i]],[oldParent,x[i]])
                return 0
        return 1
    test = honestTest()
    for entry in newParents:
        branches = np.vstack([branches, entry])
    branches = np.unique(branches, axis=0)
    slotSet = set()
    for branch in branches:
        slotSet.add(branch[0])
    branchMaxBlockNumber = {x: -1 for x in slotSet}
    maxBlockNumber = 0
    for slot in slotSet:
        for entry in branches:
            if entry[0] == slot:
                if entry[1] > branchMaxBlockNumber[slot]:
                    branchMaxBlockNumber[slot] = entry[1]
                    maxBlockNumber = max(maxBlockNumber,branchMaxBlockNumber[slot])
    branches = np.array(list(filter(lambda x: maxBlockNumber - x[1] < branchDepth, list(branchMaxBlockNumber.items()))))
    if test<1:
        newHeads = []
        for branch in branches:
            if branch[1] == maxBlockNumber:
                newHeads.append(branch)
        print("H:"+str(len(newHeads)))
        honestHeads = np.empty([1,3])
        for newHead in newHeads:
            honestHeads = np.vstack([honestHeads,[newHead[0],(1.0-alpha)/len(newHeads),newHead[1]]])
    i = i+1

# Plot the tracer points of all branches
i = 0
j = 0
for n in np.arange(N):
    for m in np.arange(N):
        if gamma>0:
            if trails[i,j] < gamma+1 and trails[i,j] > 0:
                subPlotLeft.scatter(n,m,s=trails[i,j],c=spectrum[trails[i,j]-1].reshape(1,-1),alpha=0.5)
            if trails[i,j] > gamma:
                subPlotLeft.scatter(n,m,s=int(gamma*fb/fa),c=spectrum[int(gamma*fb/fa)].reshape(1,-1),alpha=0.5)
        else:
            if trails[i,j] > 0:
                subPlotLeft.scatter(n,m,s=1.0,c=spectrum[1].reshape(1,-1),alpha=0.5)
        i = i+1
    j = j+1
    i = 0

subPlotRight.scatter(x, ys, s=area, c=colors)
subPlotRight.set_xlabel("Slot")
subPlotRight.set_ylabel("Y")
subPlotRight.set_ylim([0.0,1.0])
subPlotRight.set_title("Nonce distribution, "+r'$\alpha$ = '+str(alpha))

maxL = 0
numBranch = 0
for branch in branches:
    maxL = max(branch[1],maxL)
    numBranch = numBranch+1
hudStr = ("Maximum block number: "+str(maxL))+("\nFinal number of branches: "+str(numBranch))+("\nParent slot / block number pairs:\n")+str(branches)
subPlotLeft.text(0.01, 0.99, hudStr, ha='left', va='top', transform=subPlotLeft.transAxes)
subPlotLeft.set_xlabel("Slot")
subPlotLeft.set_ylabel("Parent slot")
subPlotLeft.set_title("Branching diagram, depth = "+str(branchDepth))

plt.show()
