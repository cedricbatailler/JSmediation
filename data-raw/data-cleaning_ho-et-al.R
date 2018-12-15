library(haven)
library(tidyverse)

ho_et_al <-
  read_spss("data-raw/Study3_dataforYzerbyt_withcontrolcond.sav") %>%
  mutate(condition =
           case_when(condit == 1 ~ "Low discrimination",
                     condit == 2 ~ "High discrimination",
                     condit == 3 ~ "No-article control")) %>%
  select(condition,
         sdo = sdo7,
         linkedfate = lfate,
         hypodescent = hypo) %>%
  rownames_to_column("id") %>%
  select(id,
         everything()) %>%
  filter(!(condition %in% c("No-article control"))) %>%
  as.data.frame()

usethis::use_data(ho_et_al, overwrite = TRUE)
