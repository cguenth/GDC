#!/bin/bash

## INPUT
## "GDC_Tau_Simplified.m" "GDC_Fig_15_3_5_Step.json" "GDC_Tau_Simplified_Out" "GDC_Tau_Simplified_Step.txt" "GDC_Tau_Simplified_Step.out"


if [ ! -d  $HOME/GDC/"$3" ]
then
        mkdir $HOME/GDC/"$3"
fi


wolframscript -script $HOME/GDC/"$1" "$2" "$3" "$4" > "$5"

mv "$5" $HOME/GDC/"$3"
