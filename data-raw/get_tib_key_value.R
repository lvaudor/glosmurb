library(tidyverse)
tib_key_value <- readr::read_csv("data-raw/tib_key_value.csv") %>%
  dplyr::filter(priority==TRUE)
usethis::use_data(tib_key_value, overwrite=TRUE)
