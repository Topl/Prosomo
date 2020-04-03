#!/bin/bash
numHolders=8
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
echo -n "Enter known peer IpAddress > "
if read -t 10 response; then
    knownPeer=$response
else
    knownPeer="127.0.0.2"
fi
echo "Holder Index = $stakeHolderIndex"
sbt "run-main prosomo.Prosomo bootstrap.conf input{params{numHolders=$numHolders,holderIndexMin=$stakeHolderIndex,holderIndexMax=$stakeHolderIndex,knownPeer=$knownPeer}}"
