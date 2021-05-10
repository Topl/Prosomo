import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import cm
from matplotlib.ticker import LinearLocator
import multiprocessing as mp
import os

# AMS 2021:
# Automata act to extend at each opportunity by grinding on all valid slot/parent slot pairs,
# represented as branches (parent slot / block number pairs).

# branches are filtered at each time step under the following rule set:

#  1) duplicates are filtered

#  2) for each parent slot among branches one branch with the maximum block number is selected and the rest are filtered

#  3) filter branches with a block number less than the maximum block number among all branches minus the branch depth

# Plots show the advantage of nothing-at-stake grinding on all possible parent slot combinations
# Advantage is calculated relative to chain growth induced by a branch depth of 1 (honest extensions only)

# Set seed for reproducibility
np.random.seed(1234)

# Number of Slots
N = 10000
# Slots
slots = np.arange(N)
# Specify the number of f_A sample points ranging from f_B to 1.0
I = 32
# Specify the number of Gamma sample points ranging from 0 to J
J = 100

# Baseline difficulty f_B
fb = 0.05

# Set x/y axis of heatmap
faxis = np.linspace(fb,1.0,I,endpoint=True)
gaxis = np.arange(J)

# use challenger model, honest extensions
useChallenger = False

# Adversarial nonces
ys = np.random.rand(N)
# Proportion of adversarial stake
alpha = 0.5

# Maximum difference between leading block number and viable branches
# branches with a gap greater than branchDepth are cut

def mp_row(iii):
    print("Pid:"+str(os.getpid())+" working on row "+str(iii))
    data = np.empty(J)
    for jjj in np.arange(J):
        def main_loop(branchDepth):
            # Snowplow amplitude
            fa = faxis[iii]
            # Forging window in slots
            gamma = gaxis[jjj]
            # Set slope of snowplow
            c = 0.0
            if gamma > 0:
                c = fa / gamma
            # Snowplow curve
            def f(d):
                if d < gamma + 1:
                    return c * d
                else:
                    return fb
            # Staking threshold
            def phi(d, a):
                return 1.0 - (1.0 - f(d)) ** a
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
            i = 0
            for y in ys:
                newParents = []
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

                if useChallenger:
                    def honestTest(hbn):
                        out = hbn
                        for head in honestHeads:
                            if np.random.uniform(0.0, 1.0) < phi(slots[i] - head[0], head[1]):
                                newParents.append([slots[i], int(head[2] + 1), 0])
                                out = max(hbn, int(head[2] + 1))
                                if makePlots:
                                    subPlotLeft.plot([slots[i], slots[i]], [head[0], slots[i]],':')
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
                        return np.concatenate((np.asarray(b),[branchReserve[b[0]]]),axis=0)
                    branches = np.array(list(map(addReserve,list(filter(lambda x: maxBlockNumber - x[1] < branchDepth,list(branchMaxBlockNumber.items()))))))
                else:
                    branches = np.array(list(filter(lambda x: maxBlockNumber - x[1] < branchDepth,list(branchMaxBlockNumber.items()))))
                i = i + 1
            maxL = 0
            numBranch = 0
            for branch in branches:
                maxL = max(branch[1], maxL)
                numBranch = numBranch + 1
            if useChallenger:
                hudStr = ("Maximum block number: " + str(maxL)) + ("\nFinal number of branches: " + str(numBranch)) + (
                    "\n[Parent slot, block number, reserve]:\n") + str(branches)
            else:
                hudStr = ("Maximum block number: " + str(maxL)) + ("\nFinal number of branches: " + str(numBranch)) + (
                    "\n[Parent slot, block number]:\n") + str(branches)
            # print(hudStr)
            # print('gamma = '+str(gamma))
            # print('fa = '+str(fa))
            return maxL
        adversL = main_loop(10)
        honestL = main_loop(1)
        data[jjj] = adversL - honestL
    return data

if __name__ == '__main__':
    pool = mp.Pool(mp.cpu_count())
    output = pool.map(mp_row,np.arange(I)[::-1])
    pool.close()
    result = np.asarray(output)
    def forceAspect(ax,aspect):
        im = ax.get_images()
        extent =  im[0].get_extent()
        ax.set_aspect(abs((extent[1]-extent[0])/(extent[3]-extent[2]))/aspect)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    img = plt.imshow(result, cmap='terrain',interpolation='antialiased', extent=[0,J,fb,1.0])
    forceAspect(ax,aspect=1.0)
    v1 = np.linspace(result.min(), result.max(), 8, endpoint=True)
    cb = plt.colorbar(ticks=v1)
    cb.ax.set_yticklabels(["{:4.0f}".format(i) for i in v1], fontsize='7')
    cb.ax.get_yaxis().labelpad = 15
    cb.ax.set_ylabel("Advantage in blocks", rotation=270)
    ax.set_title("Adversarial Nothing-at-Stake Advantage")
    ax.set_xlabel(r'$\gamma$')
    ax.set_ylabel(r'$f_A$')
    fig.savefig("fig_001.png", bbox_inches='tight')
    plt.show()

