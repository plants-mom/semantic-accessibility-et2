##
## plots
##

here::i_am("src/plots.R")

library(here)
library(dplyr)
library(ggplot2)
library(purrr)

source(here("src/04-models_summary.R"))

cur_palette <- "Set2"

measure_summary_regions <- function(plot_data,
                                    .measure = "measure",
                                    pal = cur_palette) {

  ## This should be refactored with measure_summary

  dod <- 0.5

  plot_data %>%
    ggplot(., aes(fill = .data[[.measure]], color = .data[[.measure]])) +
    geom_linerange(aes(ymin = ll, ymax = hh, x = region),
      position = position_dodge(dod)
    ) +
    geom_linerange(aes(ymin = l, ymax = h, x = region),
      size = 1.5, show.legend = FALSE,
      position = position_dodge(dod)
    ) +
    geom_point(aes(y = m, x = region),
      size = 2, shape = 21,
      position = position_dodge(dod)
    ) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_color_brewer(palette = pal) +
    scale_fill_brewer(palette = pal)
}


make_plot_data <- compose(
  ~ mcmc_intervals_data(.x, prob = 0.5, prob_outer = 0.95, point_est = "mean"),
  ~ relabel_samples(.),
  ~ posterior_samples(., pars = "b_[^I]") # this is deprecated
  ## this should be used ~ as_draws_df(., variable = "b_[^I]", regex = TRUE)
  ## but it changes things, so have a look
)

neg_atypical_rts <- function() {
  regions_data <- function(region) {
    f <- compose(readRDS, here, paste0)
    list(
      TFD = make_plot_data(f(
        "models/totfixdur_subj_mis_r",
        region, ".rds"
      )),
      RB = make_plot_data(f(
        "models/tgdur_subj_mis_r",
        region, ".rds"
      ))
    ) %>%
      bind_rows(.id = "measure") %>%
      mutate(region = region)
  }

  list(
    regions_data(6),
    regions_data(7),
    regions_data(8)
  ) %>%
    bind_rows() %>%
    mutate(
      region = case_when(
        region == 6 ~ "critical",
        region == 7 ~ "post-critical",
        region == 8 ~ "wrap-up"
      ),
      region = as.factor(region)
    ) %>%
    measure_summary_regions()
}

main <- function() {
  neg_atypical_rts()
  ggsave(here("figs/fig-cue-based-exp2.pdf"), width = 7, height = 4.7)
}

if (sys.nframe() == 0) {
  main()
}
