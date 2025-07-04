## ---- eval = FALSE------------------------------------------------------------
#  library(rjags)
#  
#  # create JAGS model
#  mf <- "
#  model {
#  for (i in 1:10)
#  {
#    y[i] ~ dnorm(mu, 0.01);
#  }
#  mu ~ dnorm(0, 0.01)
#  }
#  "
#  
#  data <- list(y = rnorm(10))
#  
#  jm <- rjags::jags.model(
#    textConnection(mf),
#    data = data,
#    n.chains = 3)
#  
#  jags_out <- rjags::coda.samples(
#    jm,
#    variable.names = 'mu',
#    n.iter = 500)

## -----------------------------------------------------------------------------
library(MCMCvis)

## ---- eval = FALSE------------------------------------------------------------
#  MCMCsummary(jags_out, round = 2)

## ---- eval = FALSE------------------------------------------------------------
#  ##     mean   sd  2.5%   50% 97.5% Rhat n.eff
#  ## mu -0.28 2.97 -6.13 -0.14  5.22    1  1397

## ---- eval = FALSE------------------------------------------------------------
#  library(nimble)
#  
#  # create NIMBLE model
#  mf2 <- nimbleCode({
#    for (i in 1:10) {
#      y[i] ~ dnorm(mu, 0.01);
#    }
#    mu ~ dnorm(0, 0.01)
#  })
#  
#  nimble_out <- nimbleMCMC(
#    code = mf2,
#    constants = list(N = 10),
#    data = data,
#    inits = list(mu = 0),
#    nchains = 4,
#    niter = 500)

## ---- eval = FALSE------------------------------------------------------------
#  MCMCsummary(nimble_out, round = 2)

## ---- eval = FALSE------------------------------------------------------------
#  ##     mean   sd  2.5%   50% 97.5% Rhat n.eff
#  ## mu -0.03 2.99 -5.76 -0.1  5.88    1  2000

## ---- eval = FALSE------------------------------------------------------------
#  library(rstan)
#  
#  # create Stan model
#  sm <- "
#  data {
#    real y[10];
#  }
#  parameters {
#    real mu;
#  }
#  model {
#    for (i in 1:10) {
#      y[i] ~ normal(mu, 10);
#    }
#    mu ~ normal(0, 10);
#  }
#  "
#  
#  stan_out <- stan(
#    model_code = sm,
#    data = data,
#    iter = 500)

## ---- eval = FALSE------------------------------------------------------------
#  MCMCsummary(stan_out, round = 2)

## ---- eval = FALSE------------------------------------------------------------
#  ##       mean   sd  2.5%   50% 97.5% Rhat n.eff
#  ## mu   -0.51 2.82 -6.06 -0.36  5.07 1.01   414
#  ## lp__ -0.45 0.61 -2.27 -0.23 -0.01 1.00   508

## ---- message = FALSE---------------------------------------------------------
MCMCsummary(MCMC_data)

## ---- message = FALSE---------------------------------------------------------
MCMCsummary(MCMC_data, round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha', 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha[1]', 
            ISB = FALSE, 
            exact = TRUE, 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha\\[1\\]', 
            ISB = FALSE, 
            exact = FALSE, 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'all', 
            excl = 'alpha', 
            ISB = FALSE, 
            exact = FALSE, 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha', 
            Rhat = TRUE, 
            n.eff = TRUE, 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha', 
            Rhat = TRUE, 
            n.eff = TRUE, 
            probs = c(0.1, 0.5, 0.9), 
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha', 
            Rhat = TRUE, 
            n.eff = TRUE, 
            HPD = TRUE, 
            hpd_prob = 0.8,
            round = 2)

## -----------------------------------------------------------------------------
MCMCsummary(MCMC_data, 
            params = 'alpha', 
            Rhat = TRUE, 
            n.eff = TRUE, 
            round = 2, 
            func = function(x) ecdf(x)(-10), func_name = "ecdf-10",
            pg0 = TRUE)

## -----------------------------------------------------------------------------
MCMCpstr(MCMC_data, 
         params = 'alpha', 
         func = mean,
         type = 'summary')

## -----------------------------------------------------------------------------
MCMCpstr(MCMC_data, func = function(x) quantile(x, probs = c(0.01, 0.99)))

## -----------------------------------------------------------------------------
ex <- MCMCpstr(MCMC_data, type = 'chains')
dim(ex$alpha)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCtrace(MCMC_data, 
          params = c('beta[1]', 'beta[2]', 'beta[3]'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCtrace(MCMC_data, 
          params = 'beta', 
          type = 'density', 
          ind = TRUE, 
          pdf = FALSE)

## ---- eval = FALSE, fig.align = 'center'--------------------------------------
#  MCMCtrace(MCMC_data,
#            pdf = TRUE,
#            open_pdf = FALSE,
#            filename = 'MYpdf',
#            wd = 'DIRECTORY_HERE')

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCtrace(MCMC_data, 
          params = c('beta[1]', 'beta[2]', 'beta[3]'), 
          ISB = FALSE, 
          exact = TRUE, 
          iter = 100, 
          ind = TRUE, 
          pdf = FALSE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
# note that the same prior used for all parameters
# the following prior is equivalent to dnorm(0, 0.001) in JAGS
PR <- rnorm(15000, 0, 32)

MCMCtrace(MCMC_data, 
          params = c('beta[1]', 'beta[2]', 'beta[3]'),
          ISB = FALSE,
          exact = TRUE,
          priors = PR,
          pdf = FALSE,
          Rhat = TRUE,
          n.eff = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCtrace(MCMC_data, 
          params = c('beta[1]', 'beta[2]', 'beta[3]'),
          ISB = FALSE,
          exact = TRUE,
          priors = PR,
          pdf = FALSE,
          post_zm = FALSE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
PPO <- MCMCtrace(MCMC_data,
                 params = c('beta[1]', 'beta[2]', 'beta[3]'),
                 ISB = FALSE,
                 exact = TRUE,
                 priors = PR,
                 plot = FALSE,
                 PPO_out = TRUE)
PPO

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCtrace(MCMC_data,
          params = c('beta[1]', 'beta[2]', 'beta[3]'),
          ISB = FALSE,
          exact = TRUE,
          priors = PR,
          pdf = FALSE,
          Rhat = TRUE,
          n.eff = TRUE,
          xlab_tr = 'This is the x for trace',
          ylab_tr = 'This is the y for trace',
          main_den = 'Custom density title',
          lwd_den = 3,
          lty_pr = 2,
          col_pr = 'green',
          sz_txt = 1.3,
          sz_ax = 2,
          sz_ax_txt = 1.2,
          sz_tick_txt = 1.2,
          sz_main_txt = 1.3)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
# generating values for each parameter used to simulate data
GV <- c(-10, -5.5, -15)

MCMCtrace(MCMC_data,
          params = c('beta[1]', 'beta[2]', 'beta[3]'),
          ISB = FALSE,
          exact = TRUE,
          gvals = GV,
          pdf = FALSE)

## -----------------------------------------------------------------------------
ex <- MCMCchains(MCMC_data, params = 'beta')

## -----------------------------------------------------------------------------
apply(ex, 2, mean)

## -----------------------------------------------------------------------------
ex2 <- MCMCchains(MCMC_data, 
                  params = 'beta',  
                  mcmc.list = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         ci = c(50, 90))

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         ci = c(50, 80), 
         HPD = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         ref_ovl = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         rank = TRUE, 
         xlab = 'PARAMETER ESTIMATE', 
         guide_lines = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         rank = TRUE, 
         horiz = FALSE, 
         ylab = 'PARAMETER ESTIMATE')

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(object = MCMC_data, 
         object2 = MCMC_data2, 
         params = 'beta', 
         offset = 0.1,
         ref_ovl = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = 'beta', 
         xlim = c(-60, 40),
         xlab = 'My x-axis label',
         main = 'MCMCvis plot',
         labels = c('First param', 'Second param', 'Third param', 
                    'Fourth param', 'Fifth param', 'Sixth param'),
          col = c('red', 'blue', 'green', 'purple', 'orange', 'black'),
          sz_labels = 1.5,
          sz_med = 2,
          sz_thick = 7,
          sz_thin = 3,
          sz_ax = 4,
          sz_main_txt = 2)

## -----------------------------------------------------------------------------
# note that the same prior used for all parameters
# the following prior is equivalent to dnorm(0, 0.001) in JAGS
PR <- rnorm(15000, 0, 32)

PPO <- MCMCtrace(MCMC_data,
                 params = c('beta[1]', 'beta[2]', 'beta[3]'),
                 ISB = FALSE,
                 exact = TRUE,
                 priors = PR,
                 plot = FALSE,
                 PPO_out = TRUE)

## ---- fig.width = 7, fig.height = 5, fig.align = 'center'---------------------
MCMCplot(MCMC_data, 
         params = c('beta[1]', 'beta[2]', 'beta[3]'),
          ISB = FALSE,
          exact = TRUE,
          xlim = c(-60, 35))
        
# each parameter is a y-unit of 1
for (i in 1:NROW(PPO)) {
  text(x = 10, y = NROW(PPO) - i + 1, paste0('PPO: ', PPO[i,2], '%'), pos = 4, col = 'red')
}

## ---- eval = FALSE------------------------------------------------------------
#  MCMCdiag(model_fit,
#           round = 3,
#           file_name = 'model-summary-YYYY-MM-DD',
#           dir = 'Results',
#           mkdir = 'model-YYYY-MM-DD',
#           add_field = '1.0',
#           add_field_names = 'Data version',
#           save_obj = TRUE,
#           obj_name = 'model-fit-YYYY-MM-DD',
#           add_obj = list(DATA, sessionInfo()),
#           add_obj_names = c('model-data-YYYY-MM-DD', 'session-info-YYYY-MM-DD'),
#           cp_file = c('model.stan', 'fit-model.R'),
#           cp_file_names = c('model-YYYY-MM-DD.stan, fit-model-YYYY-MM-DD.R'))
#  

