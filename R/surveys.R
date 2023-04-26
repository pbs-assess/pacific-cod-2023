library(tidyverse)
library(gfdata)
library(gfplot)

surv_index <- dat$survey_index %>%
  dplyr::filter(survey_abbrev %in%
                  c("SYN QCS", "SYN WCVI", "SYN HS", "SYN WCHG", "OTHER HS MSA")) %>%
  dplyr::select(survey_abbrev, year, biomass, re, lowerci, upperci, num_sets, num_pos_sets) %>%
  dplyr::mutate(lowerci = round(lowerci/1000, 1), upperci = round(upperci/1000, 1),
                biomass = round(biomass/1000, 1), re = round(re, 2)) %>%
  dplyr::rename(`Survey abbrev.` = survey_abbrev, Year = year, Biomass = biomass,
                CV = re, `Lower CI` = lowerci, `Upper CI` = upperci, Sets = num_sets, `Positive sets` = num_pos_sets) %>%
  dplyr::arrange(`Survey abbrev.`, Year)

write_csv(surv_index, here::here("data/generated/all_surveys.csv"))
