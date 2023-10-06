##
## helper functions and summaries for the fitted brms and rstan models
##

here::i_am("src/04-models_summary.R")

library(here)
library(fs)
library(dplyr)
library(brms)
library(bayesplot)
library(purrr)
library(tibble)
library(ggplot2)
library(rstan)
library(readr)

list_rename <- function(.x, ..., .strict = TRUE) {
  ## taken from:
  ## https://github.com/tidyverse/purrr/issues/804#issuecomment-729070112
  pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
  names(.x)[pos] <- names(pos)
  .x
}

msummary <- function(model_list, id = "region") {
  map(model_list, ps_rename) %>%
    bind_rows(.id = id)
}

ps_rename <- compose(
  ~ relabel_summary(.),
  ~ posterior_summary(., pars = "^b_")
)

make_plot <- compose(
  ~ mcmc_intervals(.x, prob = 0.5, prob_outer = 0.95),
  ~ relabel_samples(.),
  ~ posterior_samples(., pars = "^b_[^I]")
)

## This is a bit ugly.
##
## .return argument controls which model to use to create plots
## "available" uses models from the models dir which match a var_name.
## In that case var_name is the regex pattern prefixed by with /
## and data_list can be empty.
##
## "full_models" will try to get the full models using the
## function from the 03-models.R.
## Note that, this might result in additional models being fit
## (particularly when the data list contains regions for which no model
## is available).
post_plots <- function(var_name, data_list,
                       make_plot_func = make_plot,
                       .return = c(
                         "available",
                         "full_models",
                         "split_models"
                       )) {
  .return <- match.arg(.return)

  if (.return == "available") {
    dir_ls(here("models"), regex = paste0("/", var_name)) %>%
      set_names(compose(path_file, path_ext_remove)) %>%
      map(readRDS) %>%
      map(make_plot_func)
  } else if (.return == "full_models") {
    full_models(data_list, var_name) %>%
      map(make_plot_func)
  } else if (.return == "split_models") {
    ## TODO update
    ## this clause was used in the prev experiment
    fit_split(data_list, var_name) %>%
      map(~ map(.x, ~ make_plot_func(.x)))
  }
  ## fit_models(data_list, var_name, ...) %>%
  ##   modify_at(
  ##     "full_models",
  ##     ~ map(.x, ~ make_plot_func(.x))
  ##   ) %>%
  ##   modify_at(
  ##     "split_models",
  ##     ~ map(.x, ~ map(.x, ~ make_plot_func(.x)))
  ##   )
}

relabel_samples <- function(labelled_smpls) {
  labelled_smpls %>%
    rename_with(recode,
      b_su = "subj",
      b_ob = "obj",
      "b_su:ob" = "subj x obj",
    )
}

## TODO update
## this was used in the prev experiment
relabel_stan_sum <- function(stan_summary) {
  stan_summary %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(
      rowname =
        case_when(
          rowname == "alpha" ~ "intercept",
          rowname == "b_typic" ~ "subj",
          rowname == "b_interf" ~ "obj",
          rowname == "b_quant" ~ "quant",
          rowname == "b_interf_typic" ~ "subj x obj",
          rowname == "b_quant_typic" ~ "subj x quants",
          rowname == "b_interf_quant" ~ "obj x quants",
          rowname == "b_interf_quant_typic" ~ "subj x obj x quants",
          rowname == "prob" ~ "theta",
          TRUE ~ rowname
        )
    )
}

relabel_summary <- function(ps_summary) {
  ps_summary %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(
      rowname =
        case_when(
          rowname == "b_Intercept" ~ "intercept",
          rowname == "b_su" ~ "subj",
          rowname == "b_ob" ~ "obj",
          rowname == "b_su:ob" ~ "subj x obj",
        )
    )
}

write_summary <- function(var_name, data_list = dfs, mods = c("full", "split"),
                          ...) {
  mods <- match.arg(mods)

  if (mods == "full") {
    list(full_models = full_models(data_list, var_name)) %>%
      map(msummary) %>%
      iwalk(~ write_csv(.x, here(
        "results",
        paste0(var_name, "_", .y, ".csv")
      )))
  } else if (mods == "split") {
    ## TODO update
    ## this clause was used for the old experiment
    ## needs rewriting
    list(split_models = fit_split(data_list, var_name)) %>%
      map(~ map_dfr(., ~ msummary(., id = "quant"),
        .id = "region"
      )) %>%
      iwalk(~
        write_csv(.x, here(
          "results",
          paste0(var_name, "_", .y, ".csv")
        )))
  }
}

main <- function() {
  source(here("src/priors.R"))
  source(here("src/03-models.R"))

  dfs <- list.files(here("results"), pattern = "region[5-8].csv") %>%
    map(~ read_csv(here("results", .)))

  c("gdur", "tgdur", "rpdur", "totfixdur", "rrdur", "gbck", "rr") %>%
    walk(~ write_summary(., dfs))
}

if (sys.nframe() == 0) {
  main()
}
