#!/bin/bash
vmd -e wp5.vmd
vmd -e paraquat.vmd
convert wp5.tga paraquat.tga +append wp5_paraquat.png
rm wp5.tga paraquat.tga
