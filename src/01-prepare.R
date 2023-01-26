##
## Split conditions, select the correct items, rename
## Save dataframe without some columns
## Remove two items.
##

here::i_am("src/01-prepare.R")


library(dplyr)
library(readr)
library(here)

dataf <- read_delim(here("data/allACTFiles.txt"), delim = " ") %>%
  mutate(item = as.numeric(item))

glimpse(dataf)
unique(dataf$cond)
length(unique(dataf$subjectnr))

out <- dataf %>%
  filter(item < 41, !(item %in% c(40, 7))) %>%
  group_by(cond) %>%
  mutate(
    subj = strsplit(cond[1], "_")[[1]][1],
    obj = strsplit(cond[1], "_")[[1]][2]
  ) %>%
  ungroup()


out %>%
  select(-cond, -expname) %>%
  rename(
    subj = subjectnr, subj_cond = subj, obj_cond = obj,
    region = code
  ) %>%
  write_csv(here("results/allACTFiles_clean.csv"))
