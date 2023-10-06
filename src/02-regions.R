##
## writing different regions
##

here::i_am("src/02-regions.R")

library(here)
library(readr)
library(purrr)
library(dplyr)


dataf <- read_csv(
  here("results/allACTFiles_clean_sacccadtetime_removed.csv")
) %>%
  mutate(
    su = if_else(subj_cond == "MM", -1, 1),
    ob = if_else(obj_cond == "MM", -1, 1),
    ## additional variables
    ## rrdur = totfixdur - (gdur - gsacc), # re-reading duration
    ## corrected re-reading duration;
    ## we don't subtract gsacc because that was already done
    rrdur = totfixdur - gdur,
    rr = as.numeric(rrdur > 0)
  )


vars <- c(
  "subj", "item", "rpdur", "tgdur", "totfixdur", "gbck",
  "gdur", "rr", "rrdur"
)

dataf %>%
  select(
    region,
    all_of(vars),
    subj_cond:rr
  ) %>%
  split(.$region) %>%
  walk(~ write_csv(.x, here("results", paste0("region", .$region[1], ".csv"))))
