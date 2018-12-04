## Mikael Poul Johannesson
## 2018

## Estimates simple ideal point models (similar to the Jackman et al
## model) on Norwegian roll call data.

## Start matter ------------------------------------------------------

library(here)
library(rstan)

## Data --------------------------------------------------------------

## All availble vote tallys from the Norwegian parliament. Downloaded
## via the data.stortinget.no API. See `01_data.R` for details.
## Md5 checksum: a31307966d16e44e5f489815ff92582f
## tools::md5sum(here("data", "votes.csv"))
votes <- read.csv(
  here("data", "votes.csv"),
  stringsAsFactors = FALSE
)

## Stan needs the data in a list format.
votes_list <- list(
  J = length(unique(votes$representative)),
  M = length(unique(votes$proposal)),
  N = nrow(votes),
  j = as.integer(votes$representative),
  m = as.integer(votes$proposal),
  y = as.integer(votes$vote),
  date = votes$prop_date
)

## Md5 checksum:
## tools::md5checksum(here("models", "irt_onedim.stan"))
model_onedim <- readLines(here("models", "irt_onedim.stan"))

## Md5 checksum:
## tools::md5checksum(here("models", "irt_twodim.stan"))
model_twodim <- readLines(here("models", "irt_twodim.stan"))

## Set start values --------------------------------------------------

## votes_list$theta_init <- case_when(
##   votes$rep_party_short %in% c("R", "SV", "AP") ~ -2,
##   votes$rep_party_short %in% c("H", "FRP")      ~ 2,
##   TRUE ~ rnorm(votes_list$J, 0, 1)
## )
## theta_start <- votes_list$theta_start

## init_func <- function() {
##   list(
##     theta = theta_start,
##     beta = rnorm(M, 0, 2),
##     alpha = rnorm(M, 0, 2)
##   )
## }

## Variational inference ---------------------------------------------

fit_irt_vb_onedim <- vb(
  object =  stan_model(model_code = model_onedim),
  data = votes_list,
  seed = 2018
)

saveRDS(
  fit_irt_vb_onedim,
  file = here("output", "fit_irt_vb_onedim.rds")
)

fit_irt_vb_twodim <- vb(
  object =  stan_model(model_code = model_twodim),
  data = c(votes_list, D = 2),
  seed = 2018
)

saveRDS(
  fit_irt_vb_twodim,
  file = here("output", "fit_irt_vb_twodim.rds")
)

## Full sampling -----------------------------------------------------

fit_irt_onedim <- stan(
  model_code = model_onedim,
  data = votes_list,
  iter = 2000,
  warmup = 500,
  thin = 5,
  chains = 4,
  cores = 4,
  seed = 2018,
  verbose = TRUE
)

saveRDS(
  fit_irt_onedim,
  file = here("output", "fit_irt_onedim.rds")
)

## fit_irt_twodim <- stan(
##   model_code = model_twodim,
##   data = votes_list,
##   init = init_func.
##   iter = 5000,
##   warmup = 500,
##   thin = 5,
##   chains = 4,
##   cores = 4,
##   seed = 2018,
##   verbose = TRUE
## )

## saveRDS(
##   fit_irt_twodim,
##   file = here("output", "fit_irt_twodim.rds")
## )

## END ---------------------------------------------------------------
