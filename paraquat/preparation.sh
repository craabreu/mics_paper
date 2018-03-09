#!/bin/bash

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <base-name> <net-charge>" >&2
  exit 1
fi

base=$1
if [ ${base: -4} == ".pdb" ]; then
  echo "Error: do not include file extension" >&2
  exit 1
fi

AMBERHOME=/home/charlles/Software/anaconda3
mkdir -p antechamber && cd antechamber && echo "" > sqm.out
xterm -e "tail -f sqm.out" &
$AMBERHOME/bin/antechamber -s 2 -i ../$base.pdb -fi pdb -o ../$base.pqr -fo mpdb -c bcc -nc $2 -pl 15
$AMBERHOME/bin/antechamber -s 2 -i sqm.pdb -fi pdb -o ../$base.pqr -fo mpdb -c bcc -nc $2 -pl 15
$AMBERHOME/bin/antechamber -s 2 -i sqm.pdb -fi pdb -o ../$base.mol2 -fo mol2 -c bcc -nc $2 -pl 15
cd ..
python2 $(which playmoltools) -f pqr -i $base.pqr -o $base.playmol
