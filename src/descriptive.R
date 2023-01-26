##
## Descriptive summaries for all the regions and measures
##

here::i_am("src/descriptive.R")

library(here)
library(dplyr)
library(fs)
library(xtable)
library(purrr)
library(tidyr)
library(readr)


cleanf <- function(x) {
  ## taken from: https://stackoverflow.com/a/5472527/4190925
  oldx <- c(FALSE, x[-1] == x[-length(x)])
  # is the value equal to the previous?
  res <- x
  res[oldx] <- NA
  return(res)
}



rts_sum <- function(all_data) {
  rts <- c("rpdur", "tgdur", "totfixdur", "gdur", "rrdur")
  groups <- c("region", "subj_cond", "obj_cond")

  rts_summary <- all_data %>%
    select(all_of(rts), all_of(groups)) %>%
    pivot_longer(
      cols = !c(region, subj_cond:last_col()),
      names_to = "measure"
    ) %>%
    filter(value > 0) %>%
    group_by(region, measure, subj_cond, obj_cond) %>%
    summarise(mean = mean(value), se = sd(value) / sqrt(n())) %>%
    rename(subject = subj_cond, object = obj_cond) %>%
    mutate(
      subject = if_else(subject == "M", "match", "mis"),
      object = if_else(object == "M", "match", "mis"),
      region = as.character(region)
    )


  rts_summary$region <- cleanf(rts_summary$region)
  rts_summary$measure <- cleanf(rts_summary$measure)

  print(xtable(rts_summary),
    include.rownames = FALSE, file = here("results/rts_summary.tex")
  )
}

## probab of re-reading rr
## probab of regression abs(gbck - 2)
## gbck should be called rp


## remove 0s from gbck!
## check what does 0 in rr mean

counts_sum <- function(all_data) {
  counts <- c("gbck", "rr")
  groups <- c("region", "subj_cond", "obj_cond")

  count_summary <- all_data %>%
    select(all_of(groups), all_of(counts)) %>%
    filter(gbck != 0) %>%
    mutate(gbck = abs(gbck - 2)) %>%
    pivot_longer(
      cols = all_of(counts),
      names_to = "measure"
    ) %>%
    group_by(region, measure, subj_cond, obj_cond) %>%
    summarise(pct = (sum(value) / n()) * 100) %>%
    rename(subject = subj_cond, object = obj_cond) %>%
    mutate(
      subject = if_else(subject == "M", "match", "mis"),
      object = if_else(object == "M", "match", "mis"),
      region = as.character(region),
      measure = if_else(measure == "gbck", "rp", measure)
    )


  count_summary$region <- cleanf(count_summary$region)
  count_summary$measure <- cleanf(count_summary$measure)

  print(xtable(count_summary),
    include.rownames = FALSE, file = here("results/count_summary.tex")
  )
}

main <- function() {
  all_data <- dir_ls(here("results"), regexp = "/region[0-9].csv") %>%
    map_dfr(read_csv)
  counts_sum(all_data)
  rts_sum(all_data)
}

if (sys.nframe() == 0) {
  main()
}
