#!/usr/bin/env bash

set -euo pipefail

MOUNTPOINT="/Users/Soren/Documents/DIKU/Thesis/code"

if [[ ! -d "${MOUNTPOINT}" ]] ; then
    echo "The given directory does not exist!"
    exit
fi

echo ${MOUNTPOINT} > .revmountdir