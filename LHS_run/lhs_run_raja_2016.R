#!/usr/bin/env Rscript

###############################################################################
# script pour lancer recherche LHS sur Datarmor avec PBS qsub
# toutes les variables d’environnement sont définies dans le script PBS
###############################################################################

# normalement ce script doit être exécuté dans $PBS_O_WORKDIR

# normalement 2 packages chargés: {Rmpi} et {snow} qui mettent à dispo:
# `makeCluster`, `clusterEvalQ`, `clusterExport`, `stopCluster`
sessionInfo()

library(doParallel, include.only = c("registerDoParallel")) # pour éviter d’écraser `makeCluster`
library(foreach)

# utilisés par la partie non parallélisée (hors {foreach})
library(dplyr, warn.conflicts = FALSE)
library(data.table, include.only = c("fread"))

cl <- makeCluster()
registerDoParallel(cl)

# utilisés par la partie parallélisée (avec {foreach})
clusterEvalQ(cl, {
	library(dplyr, warn.conflicts = FALSE)
	library(data.table, include.only = c("fread"))
	library(tidyr, include.only = c("separate", "pivot_longer"))
	library(stringi, include.only = c("stri_extract_last_regex", "stri_join"))
	options(dplyr.summarise.inform = FALSE)
}) %>% # cacher des messages encombrants
suppressMessages() %>%
suppressWarnings() %>%
capture.output() %>%
invisible()

RERUN_SIMUL <- Sys.getenv("RERUN_SIMUL") %>% as.logical()

###############################################################################

parameter_names <-paste0("q", 0)



# capture en nombre par group_isis (de 0 à ...) pour une annee
obs_catch <- readRDS("InputsMACCO/obs_landKg_Raja2016.rds") %>%
 	select(group, obs)


planExperiences <- fread("out_Q/out_Q_Raja05/LHS_q.csv", data.table = FALSE, col.names = parameter_names)

	  N_SIM <-	list.files(path = "out_Q/out_Q_Raja05", pattern = "row") %>%
	length()
stopifnot(N_SIM == nrow(planExperiences))

# créer les dossiers ayant la même structure que quand on calibre
q_dirs <- sprintf("RUN_%s/i%04d", Sys.getenv("SIMUL_BASENAME"), 1:N_SIM)
for (i in q_dirs) dir.create(i, recursive = TRUE, showWarnings = FALSE)

if (RERUN_SIMUL) invisible(file.copy(
  from = sprintf("out_Q/out_Q_Raja05/row_%04d.csv", 1:N_SIM),
  #to = paste0(q_dirs, "/", parameter_names, ".csv")
  to = paste0(q_dirs, "/", "q.csv")
))

#' @title residual sum of squares function
#' @details traiter aussi le cas où obs = 0,
#'   on s’en fout le cas NA car obs inexistantes sont enlevées avec merge(sim_data, obs_data)
err_calc <- function(obs, sim) {
	res <- if_else(obs == 0, sim, (obs - sim) / obs)
	return(res ^ 2)
}

# export “biomasses fécondées” manque nom des colonnes
#biomass_cols <- c("pop", "fish_age", "zonePop", "step", "value")

#start_year <- 2018
start_year <- 2016
sep <- "_"

###############################################################################
# partie calculs parallélisés
# les messages d’erreur peuvent être très cryptiques, donc vérifier bien les codes
# remarque: éviter d’avoir plusieurs objets portant même nom, des fois MPI aime pas ça

my_progress <- function(...) cat(..., "\n", format(Sys.time(), format = "%F %Hh%M", tz = "CET", usetz = TRUE), "\n")

my_progress("Step 0:")

# lancer simul, récupérer valeurs simulées ==========================
tmp <- foreach(i = 1:N_SIM) %dopar% {

	# run ISIS --------------------------------------------
	# on est pas dans les dossiers RUN_* comme {calibrar}
	# mais toujours dans $PBS_O_WORKDIR

	# lancer simul
	nomSimul_i <- sprintf("%s_i%04d", Sys.getenv("SIMUL_BASENAME"), i)
	debug_file <- file.path(Sys.getenv("PBS_O_WORKDIR"), q_dirs[i], "debugISIS.log")
	if (RERUN_SIMUL) system2("myIsis", args = nomSimul_i, stdout = debug_file, stderr = debug_file)

	dossier_simul <- file.path(Sys.getenv("ISIS_BD"), "isis-database/simulations", nomSimul_i, "resultExports")

	# read isis csv ---------------------------------------


	# captures en nombre par groupe
	sim_catch <- file.path(dossier_simul, "CapturesPoids.csv") %>%
	  fread(data.table = FALSE) %>% # regarder nom colonnes dans fichier script java export
	  group_by(group) %>% summarise(sim = sum(value)) %>%
	  ungroup()


	

	# finish ----------------------------------------------

	return(list(
		params = unlist(planExperiences[i,]), # convert from row to named vector
		#biomasses_fecondees = biomasses_fecondees,
		# sim_catch_nbr = sim_catch_nbr,
		sim_catch = sim_catch
		# sim_catch_nbr_full = sim_catch_nbr_full
	))

}

my_progress("Step 1:")

# merge avec obs ====================================================
res_raw <- foreach(obj = tmp) %dopar% {

	catch_obs_vs_sim <- obj$sim_catch %>%
		inner_join(obs_catch, by = "group") %>%
		mutate(err = err_calc(obs, sim))

	 return(list(
	
	  params = obj$params,
		catch_obs_vs_sim = catch_obs_vs_sim
		
	))
}



###############################################################################
# finir et sauvegarder résultats

stopCluster(cl)

save_file_name <- function(suffix) paste0("res_rds/res_rds_raja2016/", suffix, ".rds")

saveRDS(res_raw, save_file_name("raw"))

#my_progress("Step 4:")
