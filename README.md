# Read me: How to reproduce the analyses ?


## Introduction

This document is a guide for reproducing the analysis performed in the paper Ricouard *et al.* (2025), *Towards implementing the Bay of Biscay multi-annual management plan: lessons from calibrating and validating the ISIS-Fish model* (submitted to *Aquatic Living Ressources*). It also contains complementary information on the calibration procedure (see section **Calibration details**) and complementary results not shown in the paper. 

In this github repository, you will find all the scripts and some basic input data necessary to reproduce the analysis. Consequently, the whole results can be obtained by re-running the process following the instructions but most outputs are not provided directly. However, a copy of our executed project, containing all processed data with all the results associated to this project (including those figuring in the paper) can be downloaded on the associated [SEANOE repository](https://doi.org/10.17882/108317).

The repository is organised as follows:

* `main_scripts` contains the core of the project with `.Rmd` scripts containing the code for the analyses of the project. Once executed, they all produce a `.html` file containing results and displaying the code with explanations.

* `R_fun` contains R scripts with the all functions used in the project 

* `input_data` contains the raw data of different sources used to extract reference data tables to be used in the calibration and validation processes. It also contains age-length conversion keys for hake and norway lobster which species are represented length-structured in the model. 

* `reference_data` contains processed data to be used directly in the calibration and validation processes. It consists in `.rds` files containing reference values for each year, organised in subfolders by variable and species or fleets.

* `Sim_data` contains the raw and processed outputs of simulations of the different models produced during the process.

* `Comparison_data` contains reference vs. simulated values obtained at the validation step for all species, fleets and variables.

* `accessibility values` contains the calibrated values of accessibility by species and by year at the different steps of the analysis.

* `TargetFactors` contains java scripts coding for target factors (along with selectivity, estimated in a previous study) in the different models. It also contains a script `FillTargetFactor_EquationsFromFiles.java` to be saved in the user's  `isis-fish-4` folder, and which is useful to import the target factors for all species and metier.

* `isis_db` contains the ISIS-Fish databases necessary to run all local simulations, both for calibration (except the LHS optimisation which are in `LHS_run` folders) and validation. These constitute versions 1 to 3b of the model. They are of two types: "calibration"  databases are set with optimised accessibility values for the current year and must be used for one-year simulations only ; "validation" databases, on the contrary, can be used for simulations of any duration and are set with mean accessibility values.

* `LHS_run` contains all the files necessary to perform the simulation and analyses associated to the LHS plan on Ifremer super calculator (Datarmor). 


* `saved_plots` contains two types of plots produced at validation stage of the analysis: (i) heatmaps of skill assessment by species (or fleet) and variables for all models and metrics, and (ii) radar plots of aggregated quality indices by species (or fleet) for all models.


## Calibration details

 The calibration process was performed in three steps:

* Simulations were run for a duration of 1 year at a time over the time-period 2015-2018, for each species separately. We first assumed  that accessibility was constant along the year. For each species and year of the plan, we drew from a Latin Hypersquare Sampling (LHS) a set of 500 values of $q_{access}$ and ran the model with that value.  Over all species, the total number of parameters to estimate was equal to : $75 \times 4$ years (2015-2018). Simulated catch by class were then compared to reference data by computing the objective function. Values of $\Phi_{\text{catch by class}}$ as a function $q_{access}(pop,cl)$, as well as optimal values of the latter are given in file `main_scripts/LHS_report.html` (see next section).

* The model was set with values  $\widehat{q_{access}}(pop,cl,y)$ minimising $\Phi_{\text{catch by class}}$, ran for a duration of 1 year and the same objective function was computed for the variables listed in the column "calibration" of table 1 (see in the article's main text). Discrepancies in landings by season were found for most species. It was then decided to set a model with seasonalised accessibility where the annual value is multiplied by the average ratio over years of reference over simulated catch of the season:
$$q_{access}(pop, cl,seas,y)=\frac{\widehat{q_{access}}(pop, cl,y) \sum_{y=2015}^{2018}\frac{C_{ref}(pop,cl,seas,y)}{C_{sim}(pop,cl,seas,y)}}{4}$$

where $C_{ref}(pop,cl,seas,y)$ and $C_{sim}(pop,cl,seas,y)$ are the reference and simulated catch in weight (except for *N. norvegicus*, in numbers) of class $cl$  of population *pop* in season *seas* of year *y* ; 
                
* Unexpected discrepancies were also found in catch by metier and country. We then set the a model with corrected target factors:
$$q_{target}(met,pop)=\frac{\widehat{q_{target}}(met,pop) \sum_{y=2015}^{2018}\frac{C_{ref}(pop,met,y)}{C_{sim}(pop,met,y)}}{4}$$

where $\widehat{q_{target}}(met,pop)$ is the previous estimation of target factors, and $C_{ref}(pop,met,y)$ and $C_{sim}(pop,met,y)$ are the reference and simulated catch of population *pop* by metier *met* in year *y*.

Sensitivity of model skills to order of steps 2 and 3 was also assessed. That is the reason why we distinguish in the next section the models 2a/3a from 2b/3b. In the first case, the seasonalisation of $q_{access}(pop, cl,seas,y)$ is performed first (as described in the article) and the correction of target factor $q_{target}(met,pop)$ in a second step. For the second case, the order of corrections is reversed. We can see in the results exposed here that this choice makes little difference in the end.

## Technical requirements

* The following R packages are used in the project and should be installed before running the analyses: `cubelyr`, `dplyr`, `FLCore`, `FLXSA`, `here`, `lattice`, `iterators`, `tidyr`, `tidyverse` 

* ISIS-Fish software is necessary to run validation simulations on a local computer. The software can be downloaded [here](https://forge.codelutin.com/projects/isis-fish/files) and complete documentation is available on the [official ISIS-Fish website](https://isis-fish.org/v4/index.html).

* Access to Datarmor supercomputer to run the simulation plan is subject to authorisation from Ifremer's *Pôle de calcul et de données marines* (PCDM). Please contact: <pcdm@ifremer.fr> to get more information on access rights.

## Run ISIS-Fish simulation on a local computer

Exports of raw simulation results are not provided in this repository for a reason of storage capacity (results are about 120GB). However, with the simulation folders provided in`Sim_data/raw`, it is possible too re-run the simulations with exactly the same settings and get the raw results. To do this, you need to have ISIS-Fish software installed (see **Technical requirements**). 

Simulations folders (named `sim_calib_mod<x>_<year>_<date>` or `sim_valid_mod<x>_<year>_<date>`) must be copied or moved to the directory `isis-fish-4/isis-database/simulations` on the user's local computer. Then, in the simulation tab of ISIS-Fish software (top right of the panel) , select the simulation you want to charge in the menu `charger une ancienne simulation`. All simulation parameters parameters will be restored. Then, change the name of the simulation (top left of the panel) and click on `Simuler`to run the simulation. A new simulation folder will be created and raw results will be stored as `.csv` files in the folder `isis-fish-4/isis-database/simulations/<simulation_name>/resultExports`.

To get more information on how to use ISIS-Fish software, for *e. g.* modifying the database and/or launching new simulations, please refer to the [official documentation](https://isis-fish.org/v4/index.html).


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

* **Assessment of model 1 by year**: Simulations of the model 1 (set with optimised accessibility values for the current year) can then be performed on a local computer, using ISS-Fish software, for a duration of one year, starting in 2015, 2016, 2017 and 2018 (see below for a guide to launch ISIS-Fish simulation on a local computer). The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod1_sim<year>_20250708`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step1_TF0q1`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step1`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_correction_step1.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. Corrections of target factors and accessibility by season are also exported. New java scripts coding for target factors are are saved in `TargetFactors/mod_2b` and new accessibility matrices (saved as `.csv`) are saved in `accessibility_values/q2_season_step2a`.

* **Assessment of model 2a by year**: Model 2a is obtained by changing accessibility values of model 1 with new accessibility matrices produced in the previous step. The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as for model 1. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod2a_sim<year>_20250716`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step2a_TF0q2`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step2a`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_correction_step2a.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. Corrections of target factors are also exported. New java scripts coding for target factors are saved in `TargetFactors/mod_3a`.

* **Assessment of model 2b by year**: Model 2b is obtained by changing target factors of model 1 with new values obtained in the previous step. This is done by importing the new target factors files (as java scripts) into the ISIS-Fish model. This operation is done by executing the script `FillTargetFactor_EquationsFromFiles.java` (to be moved from `TargetFactor`folder to the user's `isis-fish-4/isis-community_databas/scripts` directory).  Two lines must be modified: l. 32 with the location of the TargetFactors files, and l. 52 with the name of the model in which target factors are imported.
The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as for previous models. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod2b_sim<year>_250718`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step2b_TF1q1`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_correction_step2b`:
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


* **Assessment of model 3b by year**: Model 3b is obtained by changing accessibility values of model 2b with new accessibility matrices produced in the previous step. The model can then be simulated for a duration of one year, starting in 2015, 2016, 2017 and 2018, as previous models. The necessary ISIS-Fish databases are provided in `isis_db/calibration_db` (use `macco_mod3b_sim<year>_250721`). After running simulations, the  simulation folders containing results must be stored in the directory `Sim_data/raw/step3b_TF1q2`. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the file `assessment_byyear_step3b`:
```
Rscript -e "rmarkdown::render('main_scripts/assessment_correction/assessment_byyear_step3b.Rmd')"
```
The resulting `.html` file contains plots of reference vs. simulation values comparison for all species and variables organised in different tabs. This step ends the calibration process.

* **Period wise assessment of the different models**: Models 1, 2a, 2b, 3a and 3b are set with mean accessibility values and run for a period of 4 years starting in 2015 (period 1) and 2019 (period 2). The necessary ISIS-Fish databases are provided in `isis_db/validation_db` (use `macco_mod<x>_sim<year start>_<year end>_<xxxxxx>`). After running simulations, the  simulation folders containing results must be stored in the directory associated to the model. Then, the assessment species by species, variable by variable, for all years, can be performed by executing the following files :
```
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period1_step1.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period2_step1.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period1_step2a.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period2_step2a.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period1_step2b.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period2_step2b.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period1_step3a.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period2_step3a.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period1_step3b.Rmd')"
Rscript -e "rmarkdown::render('main_scripts/validation/validation_period2_step3b.Rmd')"
```
The resulting `.html` files contain plots of reference vs. simulation values comparison for all species and variables organised in different tabs. At this step, datasets with simulated vs reference values for all species, fleets and variables are saved in the `Comparison_data` directory.

* *Integrated analysis*: Finally, the integrated analysis including computation of quality metrics for all species,  fleet and variables are performed by running the script `Validation_summary.Rmd`:
```
Rscript -e "rmarkdown::render('main_scripts/validation/Validation_summary.Rmd')"
```
The resulting `.html` files contains heatmaps of quality  metrics measured for all models, as well as measures of improvement of these metrics between models. It also contains radar plots for multi-metrics synthesis. At this step, the most useful plots for the manuscript are saved in the folder `saved_plots`.

