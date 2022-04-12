# PCOtherCause

This repository houses the code used to built the OCCAM model described in
Chase et al. ([BJUI 2022](https://doi.org/10.1111/bju.15740)). It contains 3 main folders:

- App: Code used to produce the OCCAM app at occam-cap.org

- Code: Code used to build and validate OCCAM

- Data: Training data used to build OCCAM

For reproducing the OCCAM analysis, the Code folder will be the primary workhorse. Interested users will need to run the scripts in Code in the following order:

- OCM_trainingdata.Rmd: this will assemble the training data.

- OCM_modelbuilding.Rmd: based on the data assembled in OCM_trainingdata.Rmd, this script shows how OCCAM was trained and built.

- OCM_modelvalidation.Rmd: based on the models produced in OCM_modelbuilding.Rmd, this script reviews how OCCAM was validated. Note that this script can't actually be run, because the validation data cannot be publicly shared. 

- OCM_figuretables.Rmd, OCM_figurestables_final.Rmd: these scripts produce an assortment of figures and tables included in the OCCAM manuscript.

- OCM_supplement.Rmd: this script produces the results included in the supplement of the OCCAM manuscript. 

For more information on how to reproduce the OCCAM analysis, please email ecchase\@umich.edu. 
