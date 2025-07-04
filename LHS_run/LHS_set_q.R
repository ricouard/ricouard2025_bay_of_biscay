###############################################################################
# LHS uniforme pour chercher bonnes valueurs init de q
###############################################################################

# plan LHS aléatoire
library(lhs, include.only = c("randomLHS"))
library(import)
# à tester aussi: improvedLHS, maximinLHS, geneticLHS, optimumLHS, create_oalhs




source("R_fun/isis_semantic_matrix.R")

n <- 500 # nb de points
nbr_q <- 58

###############################################################################



out_folder <- "LHS_run/out_Q/out_Q_Nephrops2015_test"


nb_param <- nbr_q # nb de paramètres à estimer

# on met en 10 puissance après

# Nep 2015
#borne_inf <- c(rep(-2,5),rep(-4,12),rep(-6,5),rep(-8,6),rep(-6,4),rep(-5,4),rep(-1,2),rep(-4,20)) 
#borne_sup <-  c(rep(-0,5),rep(-0,12),rep(-3,5),rep(-5,6),rep(-5,4),rep(-3,4),rep(-0,2),rep(-0,20))

# Nep 2015_test
borne_inf <- c(rep(-2,5),rep(-4,12),rep(-6,5),rep(-8,6),rep(-6,4),rep(-5,4),rep(-1,2),rep(-4,19),-3) 
borne_sup <-  c(rep(-0,5),rep(-0,12),rep(-3,5),rep(-5,6),rep(-5,4),rep(-3,4),rep(-0,2),rep(-0,19),4)

# Nep 2016
#borne_inf <- c(rep(-2,4),-1,rep(-4,12),rep(-6,5),rep(-8,10),rep(-4,4),rep(-3,5),rep(-4,15),rep(-4,2)) 
#borne_sup <-  c(rep(-0,4),0,rep(-0,12),rep(-3,5),rep(-5,10),rep(-3,4),rep(-0,5),rep(-2,15),rep(-0,2))

# Nep 2017
#borne_inf <- c(-7,rep(-3,14),rep(-6,7),rep(-8,8),rep(-6,2),rep(-4,26)) 
#borne_sup <-  c(-3,rep(-0,14),rep(-3,7),rep(-5,8),rep(-3,2),rep(-0,26))

# Nep 2018
#borne_inf <- c(-7,rep(-3,14),rep(-6,7),rep(-8,5),-6,rep(-8,4),-6,rep(-8,3),rep(-4,22)) 
#borne_sup <-  c(-3,rep(-0,14),rep(-3,7),rep(-5,5),-3,rep(-5,4),-4,rep(-5,3),rep(-0,22))


set.seed(42)
lhs_random.in <- randomLHS(n, nb_param)

# remise à l’échelle des paramètres en utilisant les bornes
A <- matrix(borne_inf, nrow = n, ncol = length(borne_inf), byrow = TRUE)
# C <- matrix(borne_sup - borne_inf, nrow = n, ncol = length(borne_inf), byrow = TRUE)
C <- t(borne_sup - t(A)) # it’s incorrect to do `C <- borne_sup - A`

planExperiences <- 10^(A + lhs_random.in * C)
# zeros :

planExperiences[,c(c(1,3,29,30,32,34,35,36))] <- 0 # Nep 2015
#planExperiences[,c(c(1,3,36))] <- 0 # Nep 2016
#planExperiences[,c(c(3,36))] <- 0 # Nep 2017, 2018


write.table(
	planExperiences, file = file.path(out_folder, "LHS_q.csv"),
	sep = ";", col.names = FALSE, row.names = FALSE
) # pas de nom colonnes pour au cas où utiliser avec script java plan simul

# matrices en csv pour utiliser avec la règle chgmt q comme avec la calibration
# cf `ChgmtQ_variable.java`
for (i in seq_len(nrow(planExperiences))) { # i <- 1L
	tmp_df <- data.frame(
		java.lang.Integer = 1:nb_param - 1, # java array start at 0
		val = as.matrix(planExperiences)[i,]
	)
	file_name <- file.path(out_folder, sprintf("row_%04d.csv", i))
	write_as_semantic_matrix(
		tmp_df, file_name,
		val_col = "val", var_cols = "java.lang.Integer", complete = FALSE
	)
}
