#!/bin/bash
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
stakeHolderAddress=$(dig @resolver1.opendns.com ANY myip.opendns.com +short)
echo "Declared Address = $stakeHolderAddress"
echo "Holder Index = $stakeHolderIndex"
inputString="input{params{holderIndexMin=$stakeHolderIndex,holderIndexMax=$stakeHolderIndex}} input{scorex{network{declaredAddress=\"$stakeHolderAddress:9084\",upnpEnabled=yes}}}"
sbt "run-main prosomo.Prosomo bootstrap.conf $inputString"
