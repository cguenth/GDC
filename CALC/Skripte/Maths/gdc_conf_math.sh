#!/bin/bash

## INPUT
## "GDC_CONF.m" "Rev" "Pers" "Conf" "GDC_Tau_Simplified_Step.txt" " gdc_pos_Step.txt" 
## "GDC_Sigma_N_Step.txt" "GDC_Rev_Pers_Conf" 

if [ ! -d  $HOME/GDC/"$8" ]
then
        mkdir $HOME/GDC/"$8"
fi


wolframscript -script $HOME/GDC/"$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9.txt" > "$9.out"

mv "$9.out" $HOME/GDC/"$8"
