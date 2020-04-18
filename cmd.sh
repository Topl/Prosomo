#!/bin/bash
history -r cmd.history
set -o vi
CMD=""
pth="command"
nam="/cmd"
while [ "$CMD" != "q" ]
do
    echo "Enter command (q to exit) :"
    read -e CMD
    history -s "$CMD"
	if [ "$CMD" == "plot" ]
	then
	    python ./src/main/python/obPlot.py
	elif [ "$CMD" == "plot_graph" ]
	then
	    python ./src/main/python/obGraph.py
    elif [ "$CMD" == "clean" ]
    then
        rm -r -f data/*
        rm -r -f data_*
	elif [ "$CMD" != "q" ]
	then
        mkdir -p "$pth"
        rm -f "$pth$nam"
        touch "$pth$nam"
        echo "$CMD" >> "$pth$nam"
	else
        rm -f "$pth$nam"
	fi
done
history -w cmd.history
