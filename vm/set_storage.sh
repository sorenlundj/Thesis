#! /usr/bin/env bash

msg="Setting storage to"
hd=16384

echo "${msg} ${hd}"

VBoxManage modifyhd ../../../../VirtualBox\ VMs/compSys19/compSys19-v1.1-debian-64bit-disk001.vdi --resize "${hd}"
