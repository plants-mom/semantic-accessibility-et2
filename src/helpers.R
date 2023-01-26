##
##
##

here::i_am("src/helpers.R")

library(here)
library(dplyr)
library()


 get_ratio <- function(x) {
  ifelse(abs(1 / floor(1/x) - x) < abs(1 / ceiling(1 / x) - x),
         floor(1 / x), ceiling(1 / x))
}
