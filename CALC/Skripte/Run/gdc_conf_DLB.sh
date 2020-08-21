#!/bin/bash

## usage : 
## ./gdc_run.sh

cd $HOME/GDC/Work
cp $HOME/GDC/gdc_conf_job_template.sh .

for i in {"S0","S1","S2","S3","S4","S5","S6","S7","S8"}; do
    cat gdc_conf_job_template.sh | sed -e "s/Rev/AR/g; s/Pers/DLB/g; s/CM/ALL/g; s/Step/${i}/g " >  "gdc_CONF_AR_DLB_${i}_job.sh"
    chmod u+rx "gdc_CONF_AR_DLB_${i}_job.sh"
    ./"gdc_CONF_AR_DLB_${i}_job.sh"
done
