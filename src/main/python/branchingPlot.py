import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

#AMS 2021: Visualization of nothing-at-stake branching

# Set seed for reproducibility
#np.random.seed(123)

# Number of Slots
N = 100
# Slots
x = np.arange(N)
# Nonces
ys = np.random.rand(N)
# Proportion of adversarial stake
alpha = 1.0
# Forging window
gamma = 15
# Snow plow amplitude
fa = 0.5
# Set slope of snowplow
c = 0.0
if gamma>0:
    c = fa/gamma
# Baseline difficulty
fb = 0.05
# Maximum difference between leading block number and viable branches
# branches that don't meet this threshold are cut
branchDepth = 3

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
parentSlots = np.zeros((1,2),dtype=int)

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
    for parentSlot in parentSlots:
        oldParent = parentSlot[0]
        if y < phi(x[i]-parentSlot[0],alpha):
            newParents.append([parentSlot[0],parentSlot[1]])
            parentSlot[0] = x[i]
            parentSlot[1] = parentSlot[1]+1
            subPlotLeft.plot([x[i],x[i]],[oldParent,parentSlot[0]])
        if gamma>0:
            trails[oldParent,int(x[i])] = int(min(x[i]-oldParent,gamma+1))
        else:
            trails[oldParent,int(x[i])] = int(x[i]-oldParent)
    for entry in newParents:
        parentSlots = np.vstack([parentSlots,entry])
    parentSlots = np.unique(parentSlots, axis=0)
    slotSet = set()
    for parentSlot in parentSlots:
        slotSet.add(parentSlot[0])
    parentSlotMaxBlockNumber = {x: -1 for x in slotSet}
    maxBlockNumber = 0
    for slot in slotSet:
        for entry in parentSlots:
            if entry[0] == slot:
                if entry[1] > parentSlotMaxBlockNumber[slot]:
                    parentSlotMaxBlockNumber[slot] = entry[1]
                    maxBlockNumber = max(maxBlockNumber,parentSlotMaxBlockNumber[slot])
    parentSlots = np.array(list(filter(lambda x: maxBlockNumber-x[1] < branchDepth ,list(parentSlotMaxBlockNumber.items()))))
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
for parentSlot in parentSlots:
    maxL = max(parentSlot[1],maxL)
    numBranch = numBranch+1
hudStr = ("Maximum block number: "+str(maxL))+("\nFinal number of branches: "+str(numBranch))+("\nParent slot / block number pairs:\n")+str(parentSlots)
subPlotLeft.text(0.01, 0.99, hudStr, ha='left', va='top', transform=subPlotLeft.transAxes)
subPlotLeft.set_xlabel("Slot")
subPlotLeft.set_ylabel("Parent slot")
subPlotLeft.set_title("Branching diagram, depth = "+str(branchDepth))

plt.show()
