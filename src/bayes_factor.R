##
## Computing bayes factor, checking its stability and sensitivity
## to the choice of priors
##

here::i_am("src/bayes_factor.R")

library(here)
library(fs)
library(memuse) ## this is just for me
library(brms)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(rstan)
library(bridgesampling)
options(mc.cores = parallel::detectCores())

source(here("src/priors.R"))

models <- function(dv_name, region_no, subj_condition,
                   prior_mu = 0, prior_sigma = 1) {
  message(paste(
    "\n", "Fitting", dv_name, "region", region_no,
    "subj_cond", subj_condition,
    "mu", prior_mu, "sigma", prior_sigma, "\n"
  ))
  mdata <- read_csv(here("results/combined_data.csv")) %>%
    filter(
      region == region_no,
      subj_cond == subj_condition,
      .data[[dv_name]] > 0
    ) %>%
    select(-region)


  frm <- formula(~ 1 + ob +
    (1 + ob | item) + (1 + ob | subj)) |>
    update.formula(paste0(dv_name, "~ . "))

  frm_null <- formula(~ 1 +
    (1 + ob | item) + (1 + ob | subj)) |>
    update.formula(paste0(dv_name, "~ . "))

  ps <- prior_string(paste0("normal(", prior_mu, ", ", prior_sigma, ")"),
    class = "b"
  )
  priors[which(priors$class == "b"), ] <- ps
  priors_null <- priors[-which(priors$class == "b"), ]


  params <- c(
    dv_name,
    region_no,
    subj_condition,
    prior_mu,
    prior_sigma
  ) %>%
    set_names(c(
      "dv_name", "region_no", "subj_condition", "prior_mu",
      "prior_sigma"
    ))

  m <- brm(frm,
    family = lognormal(),
    prior = priors,
    iter = 4000,
    warmup = 2000,
    data = mdata,
    file = here(
      "models",
      paste0(
        "bf_", dv_name, "subj_",
        tolower(subj_condition),
        region_no, "m", prior_mu, "sd", prior_sigma
      )
    )
  )

  m_null <- brm(frm_null,
    family = lognormal(),
    prior = priors_null,
    iter = 4000,
    warmup = 2000,
    data = mdata,
    file = here("models", paste0(
      "bf_null_", dv_name, "subj_",
      tolower(subj_condition), region_no
    ))
  )

  return(list(m1 = m, m_null = m_null, params = params))
}

# computes bayes factor iters times
bf_stability <- function(samples,
                         iters, dv_name, region_no, p_mu, p_sigma, subj_c) {
  ms <- models(dv_name,
    region_no,
    subj_condition = subj_c, prior_sigma = p_sigma, prior_mu = p_mu
  )
  model1 <- ms$m1
  model1n <- ms$m_null
  params <- ms$params
  rm(ms)

  results <- tibble(
    .rows = iters,
    dv_name = params["dv_name"],
    region = as.numeric(params["region_no"]),
    subj_cond = params["subj_condition"],
    mu = as.numeric(params["prior_mu"]),
    sigma = as.numeric(params["prior_sigma"]),
    iter = NA,
    bf_10 = NA
  )


  for (n in seq(iters)) {
    message(paste(format(Sys.time(), "%H:%M:%S"), n, "out of", iters))
    message("Memory info:")
    print(Sys.meminfo())
    message("Memory used by R:")
    print(Sys.procmem())

    model1_up <- update(model1,
      iter = samples, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
    )

    stopifnot(get_num_divergent(model1_up$fit) == 0)
    m1_mlk <- bridge_sampler(model1_up)
    rm(model1_up)

    model1n_up <- update(model1n,
      iter = samples, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
    )
    stopifnot(get_num_divergent(model1n_up$fit) == 0)
    m1n_mlk <- bridge_sampler(model1n_up)
    rm(model1n_up)

    bf_score <- bayes_factor(m1_mlk, m1n_mlk)
    results[n, c("iter", "bf_10")] <- tibble_row(n, bf_score$bf)
    print(results)
    write_csv(
      results,
      here("results", paste0(
        "tmp_bf_", dv_name,
        region_no, "_mu", p_mu, "_sd", p_sigma, ".csv"
      ))
    )
  }
  if (!file_exists(here("results/bayes_factor.csv"))) {
    ## we want column names on the first run
    write_csv(results, here("results/bayes_factor.csv"))
  } else {
    write_csv(results, here("results/bayes_factor.csv"), append = TRUE)
  }
  rm(model1, model1n) # is this even needed?
  gc()
}


## checking sensitivity to the choice of priors
sensitivity <- function() {
  params <- expand_grid(
    dv_name = c("totfixdur", "tgdur"),
    region_no = c(6, 7),
    samples = 12000, iters = 5, subj_c = c("MATCH", "MIS"),
    p_mu = c(0, 0, 0, -0.03, -0.027)
  ) %>%
    mutate(
      p_sigma = rep(c(1, 0.1, 0.03, 0.009, 0.009), 8)
    )
  ## this might need to be run in batches
  ## remove the rows which were run like this: params <- params[-c(1:18), ]

  params %>%
    filter(dv_name == "totfixdur", subj_c == "MIS", region_no == 7) %>%
    pwalk(., bf_stability)
}


main <- function() {
  sensitivity()
}

if (sys.nframe() == 0) {
  main()
}
