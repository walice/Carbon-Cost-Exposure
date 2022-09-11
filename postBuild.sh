#!/bin/bash

cd ~/work

curl -L -o Data_rep.zip https://www.dropbox.com/sh/9rcid737t5itegm/AABUO6g0exmkrawBL1skqnE8a?dl=1

unzip ~/work/Data_rep.zip -d ~/work/Data_rep

mkdir ~/work/Results_rep/ ~/work/Figures_rep/

chmod u+x replicate.sh
./replicate.sh