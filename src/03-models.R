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


##
## if name == TRUE, give the model file a unique name
## if name == FALSE, name the model file: variable_r<region_number>
## if name is a string, name the model file: string_variable_r<region_number>
##
full_models <- function(data_list, dv_name, .priors, remove_zeros = TRUE,
                        .family = NULL,
                        optimize_mem = FALSE,
                        name = FALSE) {
  frm <- formula(~ 1 + su * ob +
    (1 + su * ob | item) +
    (1 + su * ob | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  sel_data <- prepare_data(data_list, dv_name, remove_zeros)

  if (name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else if (name == FALSE) {
    mname <- paste(dv_name, "r", sep = "_")
  } else {
    mname <- paste(name, dv_name, "r", sep = "_")
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
## fit models split by subject condition
##
split_models <- function(data_list, dv_name,
                         .priors = priors,
                         remove_zeros = TRUE, .family = NULL) {
  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  sel_data <- prepare_data(data_list, dv_name, remove_zeros)

  frm <- formula(~ 1 + ob + (1 + ob | item) + (1 + ob | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  split_ms <- sel_data %>%
    map(~ select(., -su)) %>%
    map(~ split(., .$subj_cond)) %>%
    map(~ set_names(., ~ if_else(. == "M", "subj_match", "subj_mis"))) %>%
    map(~ imap(., ~ brm(frm,
      family = .family, prior = .priors, iter = 4000,
      data = .x, file = here("models", paste0(
        dv_name, "_", .y, "_r",
        .x$region[1]
      ))
    )))
  return(split_ms)
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

fit_split_models <- function(data_list) {
  c("totfixdur", "tgdur") %>%
    walk(~ split_models(data_list, ., .priors = priors))
}

fit_main_measures <- function(data_list) {
  c("rrdur", "totfixdur", "gdur", "rpdur", "tgdur") %>%
    walk(~ full_models(data_list, ., .priors = priors, optimize_mem = TRUE))
}

fit_count_measures <- function(data_list) {
  ## 0 here is missing data
  map(data_list, ~ filter(., gbck != 0)) %>%
    map(., ~ mutate(., gbck = abs(gbck - 2))) %>%
    full_models(., "gbck",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  ## here 0 is meaningful
  full_models(data_list, "rr",
    .priors = priors_binom,
    remove_zeros = FALSE, optimize_mem = TRUE
  )
}

fit_combined_data <- function() {
  combd <- read_csv(here("results/combined_data.csv")) %>%
    mutate(
      # we have to add this because prepare_data needs subj_cond
      "subj_cond" = if_else(su == 1, "MATCH", "MIS"),
      "obj_cond" = if_else(ob == 1, "MATCH", "MIS")
    ) %>%
    relocate(subj_cond, .before = ob) %>%
    split(.$region)


  c("totfixdur", "tgdur") %>%
    walk(~ full_models(combd, .,
      .priors = priors, optimize_mem = TRUE,
      name = "comb"
    ))
}


main <- function() {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[5-8].csv") %>%
  ## dfs <- list.files(here("results"), pattern = "region[7-8].csv") %>%
    map(~ read_csv(here("results", .)))

  ## this might needed to be done in multiplie stages
  fit_main_measures(dfs)
  fit_count_measures(dfs)
  fit_combined_data()
  fit_split_models(dfs)
}


if (sys.nframe() == 0) {
  main()
}
