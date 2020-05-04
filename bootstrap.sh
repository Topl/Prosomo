#!/bin/bash
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
echo "Holder Index = $stakeHolderIndex"
stakeHolderAddress=$(dig @resolver1.opendns.com ANY myip.opendns.com +short)
echo "Declared Address = $stakeHolderAddress"
sbt "run-main prosomo.Prosomo bootstrap.conf \
input{params{\
myAddress=\"$stakeHolderAddress\",\
holderIndexMin=$stakeHolderIndex,\
holderIndexMax=$stakeHolderIndex,\
dataFileDir=data_$stakeHolderIndex\
}}"
