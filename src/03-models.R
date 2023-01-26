##
## bayesian models
##

here::i_am("src/03-models.R")

library(here)
library(dplyr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())


full_models <- function(data_list, dv_name, .priors, remove_zeros = TRUE,
                        .family = NULL,
                        optimize_mem = FALSE,
                        unique_name = FALSE) {
  frm <- formula(~ 1 + su * ob +
    (1 + su * ob | item) +
    (1 + su * ob | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  sel_data <- prepare_data(data_list, dv_name, remove_zeros)

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }

  full_ms <- sel_data %>%
    map(~ brm(frm,
      family = .family,
      prior = .priors,
      iter = 4000, data = .x,
      file = here("models", paste0(mname, .x$region[1]))
    ))


  if (optimize_mem == TRUE) {
    rm(full_ms, sel_data)
    gc()
  } else {
    rm(sel_data)
    return(full_ms)
  }
}

##
## selects the relevant columns, optionally removes 0s
## removing 0s needs to be optional, because for gbck and rr
## 0 means something diffferent than for the other measures.
## additionally sets names in the data list
##
prepare_data <- function(data_list, dv_name, remove_zeros) {
  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  if (remove_zeros == TRUE) {
    map(
      data_list,
      ~ select(., region:item, subj_cond:last_col(), all_of(dv_name))
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    map(
      data_list,
      ~ select(., region:item, subj_cond:last_col(), all_of(dv_name))
    ) %>% set_names(paste0("region_", nms))
  }
}


##
## I don't think this is needed
##
get_model <- function(dv_name, region, type = "full_models") {
  fit_model_func <- ifelse(type == "full_models", fit_full, fit_split)
  list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .))) %>%
    .[region] %>%
    fit_model_func(., dv_name) %>%
    pluck(paste0("region_", region))
}

fit_main_measures <- function(data_list) {
  c("rrdur", "totfixdur", "gdur", "rpdur", "tgdur") %>%
    walk(~ full_models(data_list, ., .priors = priors, optimize_mem = TRUE))
}

fit_count_measures <- function(data_list) {
  map(data_list, ~ filter(., gbck != 0)) %>%
    map(., ~ mutate(., gbck = abs(gbck - 2))) %>%
    full_models(., "gbck",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  full_models(data_list, "rr",
    .priors = priors_binom,
    remove_zeros = FALSE, optimize_mem = TRUE
  )
}


main <- function() {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[5-8].csv") %>%
    map(~ read_csv(here("results", .)))

  ## this might needed to be done in two stages
  fit_main_measures(dfs)
  fit_count_measures(dfs)
}


if (sys.nframe() == 0) {
  main()
}
