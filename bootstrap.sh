#!/bin/bash
numHolders=8
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
echo "Holder Index = $stakeHolderIndex"
BIND="0.0.0.0"
sbt "run-main prosomo.Prosomo bootstrap.conf \
input{params{\
inputSeed=\"prosomo_testnet\",\
settingsFilename=\"bootstrap.json\",\
numHolders=$numHolders,\
holderIndexMin=$stakeHolderIndex,\
holderIndexMax=$stakeHolderIndex,\
dataFileDir=data_$stakeHolderIndex,\
rpcPort=9085,\
bindAddress=$BIND\
}}"

