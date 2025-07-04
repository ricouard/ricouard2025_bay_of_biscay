# Read me: How to reproduce the analyses ?


## Introduction

This document is a guide for reproducing the analysis performed in the paper Ricouard *et al.* (2025), *What can be done with the ISIS-Fish model of the mixed demersal fishery in the Bay of Biscay? Lessons learned from calibrating and validating a complex model*. It is organised as follows:

*  `input_data` contains the raw data of different sources used to extract reference data tables to be used in the calibration and validation processes.

* `reference_data` contains processed data to be used directly in the calibration and validation processes. It consists in `.rds` files containing reference values for each year, organised in subfolders by variable and species or fleets.

* `R_fun` contains R scripts with the all functions used in the project 

* `LHS_run` contains all the files necessary to perform the simulation and analyses associated to the LHS plan on Ifremer super calculator (Datarmor). 

* `main_scripts` contains the core of the project with `.Rmd` scripts containing the code for the analyses of the project. Once executed, they all produce a `.html` file containing results.

## Technical requirements

* The following R packages are used in the project and should be installed before running the analyses: `cubelyr`, `dplyr`, `FLCore`, `FLXSA`, `here`, `lattice`, `iterators`, `tidyr`, `tidyverse` 

## Step by step execution of the analyses

* **Reference data generation:** First of all, you need to generate the refeerence datasets at the right format from the different sources. This is done by executing the file `data_generator.Rmd`:
```
Rscript -e "rmarkdown::render('main_scripts/data_generator.Rmd')"
```
Th `.html` output file contains a summary all datasets produced as well as explanations on the code.

* **Calibration step 1 on Ifremer's super calculator (Datarmor)**: the folder `LHS_run` must be copied on users work space on Datarmor. The different `.pbs` files at the root of the directory must be executed by using the following command:
```
qsub lhs_main_<speciesname><year>.pbs
```
The results are stored in the directory `res_rds`, which must be re imported locally for the subsequant steps of the analysis. As the user may not be able to have access to this super computer, we provide  here the outputs of the optimisation, so that he can perform the subsequent steps normally.

* **Analysis of step 1 outputs**: 