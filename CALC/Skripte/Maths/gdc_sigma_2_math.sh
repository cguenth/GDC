#!/bin/bash

## INPUT
## "GDC_Sigma_2.m" "GDC_Tau_Simplified_Step.txt" "gdc_pos_S2a.txt" "GDC_Sigma_2" "GDC_Sigma_2_Step.txt" "GDC_Sigma_2_Step.out"


if [ ! -d  $HOME/GDC/"$4" ]
then
        mkdir $HOME/GDC/"$4"
fi


wolframscript -script $HOME/GDC/"$1" "$2" "$3" "$4" "$5" > "$6"

mv "$6" $HOME/GDC/"$4"
