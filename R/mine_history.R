library(tidyverse)

mine_history_dat <- readxl::read_excel("brasil/input/mine_history_20201214.xls")


dat <- mine_history_dat %>% dplyr::filter(!is.na(`Full Work History`))

dat %>% group_by(`Activity Status`) %>% tally()
