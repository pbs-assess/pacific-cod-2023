# Run this file to create the inputs needed for the iscam model
# All generated outputs are put in the data/generated folder
# Author: Robyn Forrest (RF), Pacific Biological Station April-May 2023
# Based on previous assessments with code originally by RF, Sean Anderson (SA) and Chris Grandin (CG)

# This script can stand alone
library(lubridate)
library(tidyverse)
library(gfdata)
library(gfplot)
library(here)

french<-FALSE

rootd <- here::here()
rootd.R <- file.path(rootd, "R")
rootd.data <- file.path(rootd, "data")
rootd.index <- file.path(rootd, "index")
resultsd <- file.path(rootd.data, "results")
generatedd <- file.path(rootd.data, "generated")

if(!dir.exists(rootd.data)) dir.create(rootd.data)
if(!dir.exists(rootd.index)) dir.create(rootd.index)
if(!dir.exists(resultsd)) dir.create(resultsd)
if(!dir.exists(generatedd)) dir.create(generatedd)

# load necessary functions
source(here('R/get-data-functions.R'))
source(here('R/cpue-functions.R'))

# 1. Get the data and save into pcod-cache folder
# Get the data
source(file.path(rootd.R, "get-raw-data.R"))

#=================================================================================
# 2.Survey indices
surv_index <- dat$survey_index %>%
  # dplyr::filter(survey_abbrev %in%
  #                 c("SYN QCS", "SYN WCVI", "SYN HS", "SYN WCHG", "OTHER HS MSA")) %>%
  dplyr::filter(survey_abbrev %in% "SYN WCVI") %>%
  dplyr::select(survey_abbrev, year, biomass, re, lowerci, upperci, num_sets, num_pos_sets) %>%
  dplyr::mutate(lowerci = round(lowerci/1000, 1), upperci = round(upperci/1000, 1),
                biomass = round(biomass/1000, 1), re = round(re, 2)) %>%
  dplyr::rename(`Survey abbrev.` = survey_abbrev, Year = year, Biomass = biomass,
                CV = re, `Lower CI` = lowerci, `Upper CI` = upperci, Sets = num_sets, `Positive sets` = num_pos_sets) %>%
  dplyr::arrange(`Survey abbrev.`, Year)

write_csv(surv_index, here::here("data/generated/all_surveys.csv"))

#=================================================================================
# 2. CPUE indices: these take a long time
params <- list()
params$species_proper <- "Pacific Cod"
params$april1_year <- TRUE
# params$area <- c("5[ABCD]+", "3[CD]+")
# params$area_name <- c("5ABCD", "3CD")
params$area <- "3[CD]+"
params$area_name <- "3CD"
params$skip_single_variable_models <- FALSE

params$era <- "historic"
source(here::here("R/cpue.R"))
dfleet_hist <- dfleet
gg_cpue_hist <- gg_cpue
cpue_pred_hist <- predictions
arith_cpue_hist <- arith_cpue
m_historic <- readRDS(here::here("data/generated/cpue-models-pcod-historic.rds"))

params$era <- "modern"
source(here::here("R/cpue.R"))
dfleet_modern <- dfleet
gg_cpue_modern <- gg_cpue
cpue_pred_modern <- predictions
arith_cpue_modern <- arith_cpue
m_modern <- readRDS(here::here("data/generated/cpue-models-pcod-modern.rds"))

readr::write_csv(cpue_pred_modern, here::here("data/generated/cpue-predictions-modern.csv"))
readr::write_csv(cpue_pred_hist, here::here("data/generated/cpue-predictions-historical.csv"))

#=================================================================================
# 3. Catch data
## Example of how to view by year for 3CD:
## c3cd <- catch.3 %>%
##   group_by(year) %>%
##     summarize(canada = sum(canada_catch),
##     usa = sum(usa_catch),
##     total_catch = sum(total_catch))
catch.3 <- total.catch.yr.qtr(dat$catch,
                              areas = "3[CD]+",
                              include.usa = TRUE)
#
# catch.5 <- total.catch.yr.qtr(dat$catch,
#                               areas = "5[ABCD]+",
#                               include.usa = TRUE)
c3cd <- catch.3 %>%
        group_by(year) %>%
        summarize(canada = sum(canada_catch),
                  usa = sum(usa_catch),
                  total_catch = sum(total_catch)) %>%
        mutate(area="3CD") %>%
        filter(year>1955)

# c5abcd <- catch.5 %>%
#   group_by(year) %>%
#   summarize(canada = sum(canada_catch),
#             usa = sum(usa_catch),
#             total_catch = sum(total_catch)) %>%
#   mutate(area="5ABCD") %>%
#   filter(year>1955)

#allcatch <- rbind(c3cd,c5abcd)
allcatch <- c3cd

readr::write_csv(allcatch, here("data/generated/all-commercial-catch.csv"))

#=================================================================================
# 4. Commercial mean weights (copied from R/get-mean-weight.R)
# Get the commercial mean weights once the data has been pulled
source(file.path(rootd.R, "get-mean-weight.R"))
source(file.path(rootd.R, "get-mean-weight-survey.R"))




