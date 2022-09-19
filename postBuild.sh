#!/bin/bash

cd .

curl -L -o Data.zip https://www.dropbox.com/sh/pphd24hgzm6h7im/AAA0ndgbZU-79RNl_nJ-KiGta?dl=1

unzip ./Data.zip -d ./Data
rm ./Data.zip

mkdir ./Results/ ./Figures/

chmod u+x replicate.sh
./replicate.sh