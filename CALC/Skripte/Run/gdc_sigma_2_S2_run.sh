#!/bin/bash

## usage : 
## ./gdc_run.sh

cd $HOME/GDC/Work
cp $HOME/GDC/gdc_sigma_2_job_template.sh .



for i in "S2"; do
       cat gdc_sigma_2_job_template.sh | sed -e "s/Step/${i}/g " >  "gdc_Sigma_2_${i}_job.sh"
       chmod u+rx "gdc_Sigma_2_${i}_job.sh"
       ./"gdc_Sigma_2_${i}_job.sh"
done

