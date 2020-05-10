#!/bin/bash
echo -n "Enter holder index, or wait 10 seconds for a random index > "
if read -t 10 response; then
    stakeHolderIndex=$response
else
    stakeHolderIndex=$(((RANDOM<<15)|RANDOM))
fi
sbt "run-main prosomo.Prosomo bootstrap.conf \
input{params{\
holderIndexMin=$stakeHolderIndex,\
holderIndexMax=$stakeHolderIndex,\
settingsFilename=\"bootstrap_upnp.json\"\
}}"
