#!/bin/sh
if [ $# -ne 1 ]; then
 echo "Usage: $0 file"
 exit 1
fi
csi -s $1 |tee -a results/$1.bench
