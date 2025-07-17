# Read me: How to reproduce the analyses ?


## Introduction

This document is a guide for reproducing the analysis performed in the paper Ricouard *et al.* (2025), *What can be done with the ISIS-Fish model of the mixed demersal fishery in the Bay of Biscay? Lessons learned from calibrating and validating a complex model*. It is organised as follows:

*  `input_data` contains the raw data of different sources used to extract reference data tables to be used in the calibration and validation processes. It also contains age-length conversion keys for hake and norway lobster which species are represented length-structured in the model. 

* `reference_data` contains processed data to be used directly in the calibration and validation processes. It consists in `.rds` files containing reference values for each year, organised in subfolders by variable and species or fleets.

* `Sim_data` contains the raw and processed outputs of simulations of the different models produced during the process.

* `accessibility values` contains the calibrated values of accessibility by species and by year at the different steps of the analysis.

* `TargetFactors` contains java scripts coding for target factors (along with selectivity, estimated in a previous study) in the different models. It also contains a script `FillTargetFactor_EquationsFromFiles.java` to be saved in the user's  `isis-fish-4` folder, and which is useful to import the target factors for all species and metier.

* `isis_db` contains the ISIS-Fish databases necessary to run all local simulations, both for calibration (except the LHS optimisation which are in `LHS_run` folders) and validation. These constitute versions 1 to 3b of the model.

* `R_fun` contains R scripts with the all functions used in the project 

* `LHS_run` contains all the files necessary to perform the simulation and analyses associated to the LHS plan on Ifremer super calculator (Datarmor). 

* `main_scripts` contains the core of the project with `.Rmd` scripts containing the code for the analyses of the project. Once executed, they all produce a `.html` file containing results and displaying the code with explanations.

## Technical requirements

* The following R packages are used in the project and should be installed before running the analyses: `cubelyr`, `dplyr`, `FLCore`, `FLXSA`, `here`, `lattice`, `iterators`, `tidyr`, `tidyverse` 

## Step by step execution of the analyses

* **Reference data generation:** First of all, you need to generate the reference datasets at the right format from the different sources. This is done by executing the file `data_generator.Rmd`:
```
Rscript -e "rmarkdown::render('main_scripts/data_generator.Rmd')"
```
The `.html` output file contains a summary all datasets produced as well as explanations on the code.

* **Calibration step 1 on Ifremer's super calculator (Datarmor)**: the folder `LHS_run` must be copied on users work space on Datarmor. The different `.pbs` files at the root of the directory must be executed by using the following command:
```
qsub lhs_main_<speciesname><year>.pbs
```
The results are stored in the directory `res_rds`, which must be re imported locally for the subsequent steps of the analysis. As the user may not be able to have access to this super computer, we provide  here the outputs of the optimisation, so that he can perform the subsequent steps normally.

* **Analysis of step 1 outputs**: To perform the optimisation of accessibility values from the outputs of the LHS simulation plan and display the diagnostics, you need to execute the file `LHS_report.Rmd`:
```
Rscript -e "rmarkdown::render('main_scripts/LHS_report.Rmd')"
```
The resulting `.html` file shows by species and by year the value of the objective function for a range of accessibility values, and a comparison of reference *vs.* simulated values of catch by group for the best simulation. The accessibility profile is also displayed, including mean accessibility and estimations for each year. Values of accessibility by year by species are saved in `.csv` and semantics (readable by ISIS-Fish software) formats in the directory `accessibility_values/q1_LHS_outputs`.

* **Assessment of model 1 by year**: Simulations of the model 1 (set with optimised accessibility values) can then be performed on a local computer, using ISS-Fish software, for a duration of one year, starting in 2015, 2016, 2017 and 2018 (see below for a guide to launch ISIS-Fish simulation on a local computer). The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod1_sim<year>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step1_TF0q1`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step1`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_correction_step1.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. Corrections of target factors and accessibility by season are also exported. New java scripts coding for target factors are are saved in `TargetFactors/mod_2b` and new accessibility matrices (saved as `.csv`) are saved in `accessibility_values/q2_season_step2a`.

* **Assessment of model 2a by year**: Model 2a is obtained by changing accessibility values of model 1 with new accessibility matrices produced in the previous step. The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as for model 1. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod2a_sim<year>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step2a_TF0q2`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step2a`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_correction_step2a.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. Corrections of target factors are also exported. New java scripts coding for target factors are saved in `TargetFactors/mod_3a`.

* **Assessment of model 2b by year**: Model 2b is obtained by changing target factors of model 1 with new values obtained in the previous step. This is done by importing the new target factors files (as java scripts) into the ISIS-Fish model. This operation is done by executing the script `FillTargetFactor_EquationsFromFiles.java` (to be moved from `TargetFactor`folder to the user's `isis-fish-4/isis-community_databas/scripts` directory).  Two lines must be modified: l. 32 with the location of the TargetFactors files, and l. 52 with the name of the model in which target factors are imported.

The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as for previous models. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod2b_sim<year>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step2b_TF1q1`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step2a`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_correction_step2b.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. Seasonal corrections of accessibility are also exported. New accessibility matrices by species are saved as `.csv` in `accessibility_values/q2_season_step3b`.

* **Assessment of model 3a by year**: Model 3a is obtained by changing target factors of model 2a with new values obtained in the previous step. This is done by importing the new target factors files (as java scripts) into the ISIS-Fish model. This operation is done by executing the script `FillTargetFactor_EquationsFromFiles.java` (to be moved from `TargetFactor`folder to the user's `isis-fish-4/isis-community_databas/scripts` directory).  Two lines must be modified: l. 32 with the location of the TargetFactors files, and l. 52 with the name of the model in which target factors are imported.

The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as for previous models. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod3a_sim<year>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step3a_TF1q2`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_byyear_step3a`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_byyear_step3a.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. 


* **Assessment of model 3b by year**: Model 3b is obtained by changing accessibility values of model 2b with new accessibility matrices produced in the previous step. The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as previous models. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod3b_sim<year>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step3b_TF1q2`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_byyear_step3b`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_byyear_step3b.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. 