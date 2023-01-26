##
## Computing bayes factor, and checking its stability
##

here::i_am("src/bf_stability.R")

library(here)
library(brms)
library(dplyr)
library(readr)
library(bridgesampling)
options(mc.cores = parallel::detectCores())

source(here("src/priors.R"))

models <- function(dv_name, region_no) {
  mdata <- read_csv(here("results/bf_data.csv")) %>%
    filter(
      region == region_no, totfixdur > 0,
      tgdur > 0
    ) %>%
    select(-region)


  frm <- formula(~ 1 + ob +
    (1 + ob | item) + (1 + ob | subj)) |>
    update.formula(paste0(dv_name, "~ . "))

  frm_null <- formula(~ 1 +
    (1 + ob | item) + (1 + ob | subj)) |>
    update.formula(paste0(dv_name, "~ . "))

  priors[which(priors$class == "b"), ] <- prior(normal(0, 0.03), class = b)
  priors_null <- priors[-which(priors$class == "b"), ]

  m <- brm(frm,
    family = lognormal(),
    prior = priors,
    iter = 4000,
    warmup = 2000,
    data = mdata,
    file = here("models", paste0("bf_", dv_name, region_no))
  )

  m_null <- brm(frm_null,
    family = lognormal(),
    prior = priors_null,
    iter = 4000,
    warmup = 2000,
    data = mdata,
    file = here("models", paste0("bf_null_", dv_name, region_no))
  )

  return(list(m1 = m, m_null = m_null))
}


bf_stability <- function(samples, iters, dv_name, region_no) {
  ms <- models(dv_name, region_no)
  model1 <- ms$m1
  model1n <- ms$m_null
  rm(ms)

  results <- data.frame(matrix(vector(), iters, 2,
    dimnames = list(c(), c("iter", "bf_10"))
  ))

  for (n in seq(iters)) {
    message(paste(format(Sys.time(), "%H:%M:%S"), n, "out of", iters))

    model1_up <- update(model1,
      iter = samples, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
    )
    m1_mlk <- bridge_sampler(model1_up)
    rm(model1_up)

    model1n_up <- update(model1n,
      iter = samples, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
    )
    m1n_mlk <- bridge_sampler(model1n_up)
    rm(model1n_up)

    bf_score <- bayes_factor(m1_mlk, m1n_mlk)
    results[n, ] <- c(n, bf_score$bf)
    print(results)
    write.csv(results,
      here("results", paste0("bf_", dv_name, region_no, ".csv")),
      row.names = FALSE
    )
  }
}

main <- function() {
  smpl <- 12000
  it <- 5
  bf_stability(smpl, it, "totfixdur", 6)
  bf_stability(smpl, it, "totfixdur", 7)
  bf_stability(smpl, it, "tgdur", 6)
  bf_stability(smpl, it, "tgdur", 7)
}

if (sys.nframe() == 0) {
  main()
}
