#!/bin/bash
numHolders=8
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
#echo -n "Enter known peer IP address > "
#if read -t 10 response; then
#    knownPeer=$response
#else
#    knownPeer=""
#fi
echo "Holder Index = $stakeHolderIndex"
#PORT=$(( ((RANDOM<<15)|RANDOM) % 63001 + 2000 ))
#echo $PORT
#BIND="127.$((RANDOM % 256)).$((RANDOM % 256)).$((RANDOM % 256))"
BIND="0.0.0.0"
#echo $BIND
sbt "run-main prosomo.Prosomo bootstrap.conf \
input{params{\
inputSeed=\"prosomo_testnet\",\
settingsFilename=\"bootstrap.json\",\
numHolders=$numHolders,\
holderIndexMin=$stakeHolderIndex,\
holderIndexMax=$stakeHolderIndex,\
knownPeer=\"$knownPeer\",\
dataFileDir=data_$stakeHolderIndex,\
rpcPort=9085,\
bindAddress=$BIND\
}}"

