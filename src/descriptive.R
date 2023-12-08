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

  tbl <- xtable(rts_summary,
    caption = "Experiment 4: Mean raw reading times by measure and condition"
  )
  print(tbl,
    include.rownames = FALSE,
    tabular.environment = "longtable", print.results = FALSE,
    caption.placement = "top",
    floating = FALSE
  )
}

## probab of re-reading rr
## probab of regression abs(gbck - 2)
## gbck should be called rp


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


  tbl <- xtable(count_summary,
    caption = "Experiment 4: Count summary by measure and condition"
  )
  print(tbl,
    include.rownames = FALSE,
    tabular.environment = "longtable", print.results = FALSE,
    caption.placement = "top",
    floating = FALSE
  )
}

main <- function() {
all_data <- dir_ls(here("results"), regexp = "/region[0-9].csv") %>%
  map_dfr(read_csv) %>%
  mutate(
    region = case_when(
      region == 1 ~ "subject",
      region == 2 ~ "verb",
      region == 3 ~ "object",
      region == 4 ~ "wrap-up1",
      region == 5 ~ "pre-critical",
      region == 6 ~ "critical",
      region == 7 ~ "post-critical",
      region == 8 ~ "wrap-up2"
    ),
    region = factor(region,
      levels = c("subject", "verb", "object",
                 "wrap-up1", "pre-critical",
                 "critical", "post-critical", "wrap-up2"),
      ordered = TRUE
    )
  )
  file_conn <- file(here("results/descriptive.tex"))
  writeLines(c(
    "\\documentclass{article}",
    "\\usepackage{longtable}\n\\begin{document}",
    rts_sum(all_data),
    counts_sum(all_data),
    "\\end{document}"
  ), file_conn)
  close(file_conn)
}

if (sys.nframe() == 0) {
  main()
}
