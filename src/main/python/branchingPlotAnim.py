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
np.random.seed(123)

# Number of Slots
N = 200
# Slots
slots = np.arange(N)
# Adversarial nonces
ys = np.random.rand(N)
# Proportion of adversarial stake
alpha = 1.0
# Forging window
gamma = 15
# Snowplow amplitude
fa = 0.5
# Set slope of snowplow
c = 0.0
if gamma > 0:
    c = fa / gamma
# Baseline difficulty
fb = 0.05
# Maximum difference between leading block number and viable branches
# branches with a gap greater than branchDepth are cut
branchDepth = 6
# use challenger model, honest extensions
useChallenger = False
# calculate plots
makePlots = True


# Snowplow curve
def f(d):
    if d < gamma + 1:
        return c * d
    else:
        return fb


# Staking threshold
def phi(d, a):
    return 1.0 - (1.0 - f(d)) ** a


# Nonce distribution plot area and colors
area = []
colors = []


# Generates the spectrum of colors for trails in branching diagram
def genSpectrum(g):
    if g > 0:
        spec = cm.rainbow(np.linspace(0, 1, g))
        spec = np.append(spec, [[0.0, 0.0, 0.0, 1.0]], axis=0)
        return spec
    else:
        return np.array([[1.0, 0.0, 0.0, 1.0], [0.0, 0.0, 0.0, 1.0]])


spectrum = genSpectrum(gamma)

if makePlots:
    fig, axs = plt.subplots(1, 3, gridspec_kw={'width_ratios': [2, 1, 1]}, figsize=(16, 7))
    subPlotLeft = axs[0]
    subPlotLeft.set_aspect('equal')
    subPlotMid = axs[1]
    subPlotRight = axs[2]

leadingHonestBlockNumber = 0
if useChallenger:
    # Empty string with genesis prefix
    # rows are branches, columns are parent slot / block number / reserve
    branches = np.zeros((1, 3), dtype=int)
    honestHeads = np.array([[0, 1.0 - alpha, 0]])
else:
    # Empty string with genesis prefix
    # rows are branches, columns are parent slot / block number
    branches = np.zeros((1, 2), dtype=int)

subPlotRight.set_xlabel("Slot")
subPlotRight.set_ylabel("Y")
subPlotRight.set_ylim([0.0, 1.0])
subPlotRight.set_title("Nonce distribution, " + r'$\alpha$ = ' + str(alpha))

subPlotLeft.set_xlabel("Slot")
subPlotLeft.set_ylabel("Parent slot")
subPlotLeft.set_title("Branching diagram, depth = " + str(branchDepth))
subPlotLeft.text(0.01, 0.99, "[]", ha='left', va='top', transform=subPlotLeft.transAxes)

subPlotMid.set_xlabel("Block Number")
subPlotMid.set_ylabel("Parent slot")
subPlotMid.set_title("Branch Length")

i = 0
for y in ys:
    if makePlots:
        if gamma > 0:
            if y > phi(gamma, alpha):
                area.append(1.0)
                colors.append(spectrum[gamma])
                subPlotLeft.scatter(slots[i], slots[i], s=1.0, c=spectrum[gamma].reshape(1, -1))
            else:
                j = gamma
                while y > phi(gamma - j, alpha):
                    j = j - 1
                area.append(2.0 * j)
                colors.append(spectrum[gamma - j])
                subPlotLeft.scatter(slots[i], slots[i], s=j, c=spectrum[gamma - j].reshape(1, -1), alpha=0.5)
        else:
            if y > phi(1, alpha):
                area.append(1.0)
                colors.append(spectrum[1])
                subPlotLeft.scatter(slots[i], slots[i], s=1.0, c=spectrum[1].reshape(1, -1))
            else:
                area.append(25.0)
                colors.append(spectrum[0])
                subPlotLeft.scatter(slots[i], slots[i], s=25.0, c=spectrum[0].reshape(1, -1), alpha=0.5)
    subPlotRight.scatter(slots[i], ys[i], s=area[i], color=colors[i])

    newParents = []
    trails = np.empty((0, 3), int)
    for branch in branches:
        oldParent = branch[0]
        if y < phi(slots[i] - branch[0], alpha):
            if useChallenger:
                newParents.append([branch[0], branch[1], branch[2]])
            else:
                newParents.append([branch[0], branch[1]])
            branch[0] = slots[i]
            branch[1] = branch[1] + 1
            if useChallenger:
                branch[2] = branch[2] + 1
            if makePlots:
                subPlotLeft.plot([slots[i], slots[i]], [oldParent, slots[i]])
        if makePlots:
            if gamma > 0:
                trails = np.vstack([trails, [int(slots[i]), oldParent, int(min(slots[i] - oldParent, gamma + 1))]])
            else:
                trails = np.vstack([trails, [int(slots[i]), oldParent, int(slots[i] - oldParent)]])

    for point in trails:
        if gamma > 0:
            if gamma + 1 > point[2]:
                subPlotLeft.scatter(point[0], point[1], s=point[2], c=spectrum[point[2] - 1].reshape(1, -1), alpha=0.5)
            if point[2] > gamma:
                subPlotLeft.scatter(point[0], point[1], s=int(gamma * fb / fa),
                                    c=spectrum[int(gamma * fb / fa)].reshape(1, -1), alpha=0.5)
        else:
            if point[2] > 0:
                subPlotLeft.scatter(point[0], point[1], s=1.0, c=spectrum[1].reshape(1, -1), alpha=0.5)

    if useChallenger:
        def honestTest(hbn):
            out = hbn
            for head in honestHeads:
                if np.random.uniform(0.0, 1.0) < phi(slots[i] - head[0], head[1]):
                    newParents.append([slots[i], int(head[2] + 1), 0])
                    out = max(hbn, int(head[2] + 1))
                    if makePlots:
                        subPlotLeft.plot([slots[i], slots[i]], [head[0], slots[i]], ':')
            return out


        challengerNumber = honestTest(leadingHonestBlockNumber)
        if challengerNumber > leadingHonestBlockNumber:
            leadingHonestBlockNumber = maxBlockNumber
            newHeads = []
            for branch in branches:
                if branch[1] == leadingHonestBlockNumber:
                    newHeads.append(branch)
            honestHeads = np.empty([1, 3])
            for newHead in newHeads:
                honestHeads = np.vstack([honestHeads, [newHead[0], (1.0 - alpha) / len(newHeads), newHead[1]]])

    for entry in newParents:
        branches = np.vstack([branches, entry])

    branches = np.unique(branches, axis=0)
    slotSet = set()

    for branch in branches:
        slotSet.add(branch[0])

    branchMaxBlockNumber = {x: -1 for x in slotSet}

    if useChallenger:
        branchReserve = {x: -1 for x in slotSet}

    maxBlockNumber = 0
    for slot in slotSet:
        for entry in branches:
            if entry[0] == slot:
                if entry[1] > branchMaxBlockNumber[slot]:
                    if useChallenger:
                        branchReserve[slot] = entry[2]
                    branchMaxBlockNumber[slot] = entry[1]
                    maxBlockNumber = max(maxBlockNumber, branchMaxBlockNumber[slot])

    if useChallenger:
        def addReserve(b):
            return np.concatenate((np.asarray(b), [branchReserve[b[0]]]), axis=0)
        branches = np.array(list(map(addReserve, list(filter(lambda x: maxBlockNumber - x[1] < branchDepth,
                                                             list(branchMaxBlockNumber.items()))))))
    else:
        branches = np.array(list(filter(lambda x: maxBlockNumber - x[1] < branchDepth,
                                        list(branchMaxBlockNumber.items()))))

    branches = branches[np.argsort(branches[:, 1])]
    maxL = 0
    numBranch = 0
    for branch in branches:
        maxL = max(branch[1], maxL)
        numBranch = numBranch + 1
    if useChallenger:
        hudStr = ("Maximum block number: " + str(maxL)) + ("\nNumber of branches: " + str(numBranch)) + (
            "\n[Parent slot, block number, reserve]:\n") + str(branches)
    else:
        hudStr = ("Maximum block number: " + str(maxL)) + ("\nNumber of branches: " + str(numBranch)) + (
            "\n[Parent slot, block number]:\n") + str(branches)
    if makePlots:
        subPlotLeft.texts[-1].set_text(hudStr)
    else:
        print(hudStr)
    if makePlots:
        subPlotMid.clear()
        for branch in branches:
            subPlotMid.scatter(branch[1], branch[0])
            subPlotMid.set_xlabel("Block Number")
            subPlotMid.set_ylabel("Parent slot")
            subPlotMid.set_title("Branch Length")
    plt.savefig("slide_"+str(i)+".png")
    i = i + 1

plt.show()
