##
## Merge data from this and previous experiment
## Subset relevant variables, measures
##

here::i_am("src/data_bf.R")

library(here)
library(dplyr)
library(readr)
library(purrr)


path <- "/home/jas/surfd_sync/Documents/experiments/var/tijns_data/results/"

dvars <- c("region", "subj", "item", "tgdur", "totfixdur")


old_data <- list(
  region6 = paste0(path, "region6.csv"),
  ## 7 in the new exp is 8 in the old, this is not a mistake
  region7 = paste0(path, "region8.csv")
) %>%
  map_dfr(read_csv) %>%
  filter(
    quan_cond == "GEEN",
    typic_cond == "atypical"
  ) %>%
  select(
    all_of(dvars),
    interf, typic
  ) %>%
  mutate(
    exp = 1, region = if_else(region == 8, 7, region),
  ) %>%
  rename(su = typic, ob = interf)

new_data <- list(
  region6 = "results/region6.csv",
  region7 = "results/region7.csv"
) %>%
  map_dfr(compose(read_csv, here)) %>%
  filter(subj_cond == "MM") %>%
  select(
    all_of(dvars),
    su, ob
  ) %>%
  mutate(exp = 2,
    item = item + max(old_data$item),
    ## subj = paste(subj, "e2", sep = "_")
    subj = subj + max(old_data$subj))

bind_rows(old_data, new_data) %>%
  select(-c(su, exp)) %>%
  write_csv(here("results/bf_data.csv"))
