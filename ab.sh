sbt stage
mkdir -p /tmp/topl/alice/bin/data/
mkdir -p /tmp/topl/bob/bin/data/
mkdir -p /tmp/topl/alice/bin/time/
mkdir -p /tmp/topl/bob/bin/time/
rm -r -f /tmp/topl/alice/bin/data/*
rm -r -f /tmp/topl/bob/bin/data/*
rm -f /tmp/topl/alice/bin/time/*
rm -f /tmp/topl/bob/bin/time/*
echo $(($(date +%s%N)/1000000)) | tee /tmp/topl/alice/bin/time/t /tmp/topl/bob/bin/time/t
'cp' -fr target/universal/stage/* /tmp/topl/alice/
'cp' -fr target/universal/stage/* /tmp/topl/bob/
'cp' -fr alice.conf /tmp/topl/alice/bin/
'cp' -fr bob.conf /tmp/topl/bob/bin/
echo "to run this simulation run 'cd /tmp/topl/alice/bin/; ./prosomo alice.conf' and in a separate window 'cd /tmp/topl/bob/bin/; ./prosomo bob.conf'"
