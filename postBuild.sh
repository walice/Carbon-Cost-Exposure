#!/bin/bash

cd ~/work

curl -L -o Data.zip https://www.dropbox.com/sh/pphd24hgzm6h7im/AAA0ndgbZU-79RNl_nJ-KiGta?dl=1

unzip ~/work/Data.zip -d ~/work/Data

mkdir ~/work/Results/ ~/work/Figures/

chmod u+x replicate.sh
./replicate.sh