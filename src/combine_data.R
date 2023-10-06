##
## Merge data from this and previous experiment
## Subset relevant variables and measures
##

here::i_am("src/combine_data.R")

library(here)
library(dplyr)
library(readr)
library(purrr)


combine_data <- function() {
  dvars <- c("region", "subj", "item", "tgdur", "totfixdur", "subj_cond")

  rd_here <- compose(read_csv, here)

  old_data <- list(
    region6 = "data/exp1-region6.csv",
    ## 7 in the new exp is 8 in the old, this is not a mistake
    region7 = "data/exp1-region8.csv"
  ) %>%
    map_dfr(rd_here) %>%
    filter(quan_cond == "GEEN") %>%
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
    map_dfr(rd_here) %>%
    select(
      all_of(dvars),
      su, ob
    ) %>%
    mutate(
      exp = 2,
      item = item + max(old_data$item),
      subj = subj + max(old_data$subj)
    )

  return(bind_rows(old_data, new_data))
}

main <- function() {
  write_csv(combine_data(), here("results/combined_data.csv"))
}

if (sys.nframe() == 0) {
  main()
}
