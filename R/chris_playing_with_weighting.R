# Code to get the annual mean weight in the survey. Try to mimic the commercial mean
# weight code as much as possible

# This is for the technical working group meeting to look at whether we can
# calibrate the survey mean weight to the commercial mean weight, to possibly
# use survey mean weight going forward despite different selectivities

# June 2 2022. Robyn Forrest (RF)

# TODO: Check that the catch weighting makes sense
# Well, none of it really makes sense because length sampling has been
# length-stratified for most years of the survey!!

french <- FALSE
# source(here::here("R/all.R"))

library(dplyr)
library(ggplot2)
library(reshape2)
library(here)

# AREA <- "3CD"
AREA <- "3CD"

TYPE <- "weighted"
# TYPE <- "raw"

# L-W parameters
#3CD
if (AREA == "3CD") {
  .ALPHA <- 7.65616e-06
  .BETA <- 3.08
} else {
  #5ABCD
  .ALPHA <- 6.722839e-06
  .BETA <- 3.11
}


if (AREA == "3CD") SURVEY <- c("SYN WCVI")
if (AREA == "5ABCD") SURVEY <- c("SYN QCS", "SYN HS")

#dat <-  readRDS(here("data/pcod-cache/pacific-cod.rds"))
dat <-  readRDS(here("data/pcod-cache/survey-sets-and-samples.rds"))
#dat <- readRDS("~/Downloads/survey-sets-and-samples.rds")

# survsamps <- list()
# survsamps$survey_samples <- dat$survey_samples
# survsamps$survey_sets <- dat$survey_sets
# saveRDS(survsamps, "data/pcod-cache/survey-sets-and-samples.rds")
# test <- readRDS("data/pcod-cache/survey-sets-and-samples.rds")

catch_weight_summary <- dat$survey_sets %>%
  select(c(year, fishing_event_id, sample_id, grouping_code,density_kgpm2, catch_weight)) %>%
  filter(!is.na(sample_id))

# get survey lengths (unweighted)
# weight_calc=.ALPHA3*length^.BETA3 is Eq C.5 in the 2018 assessment
lengthwt_raw <- dat$survey_samples %>%
  filter(survey_abbrev %in% SURVEY,
         usability_code %in% c(0, 1, 2, 6),
         !is.na(length)) %>%
  select(year, fishing_event_id, sample_id, grouping_code, length, weight) %>%
  mutate(weight_calc = .ALPHA * length ^ .BETA, weight = weight / 1000) %>%
  left_join(catch_weight_summary)

# Now weight by catch
# get the mean weight in the samples, with the catch weight from the fishing event
# Equivalent to Eq C.6 in the 2018 assessment
Mean_wt_samples <- lengthwt_raw |>
  group_by(year, sample_id, grouping_code) |>
  summarize(mean_weight_calc = mean(weight_calc),
            sum_weight_calc=sum(weight_calc),
            catch_weight = first(catch_weight)) |>
  ungroup()

# now weight by depth stratum. Sort of the equivalent of Eq C.7 in the 2018 assessment,
# which weighted by sequential quarter
# only do calculated mean weight ... not enough weight obs per strata

# From Appendix C of 2018 assessment
# The mean weight (Ws) for each stratum [sequential quarter] was then calculated,
# weighted by the sample weight of Pacifc Cod (Sj ) in each SampleID (j).
# If the sample weight was recorded as data, it is used.
# Otherwise, the sum of the calculated weights from the sample is used
# Ws = Sum(Wjs*Sjs)/Sum(Sjs)
# Each Sum is over Ks, which is the number of sample ids in each stratum
# for sampleid j and stratum s

# OPtion: include sample_id in weighting
Mean_weight_stratum_with_sample_id <- Mean_wt_samples |>
  group_by(year, sample_id, grouping_code) |>
  summarize(stratum_mean_wt =
              sum(mean_weight_calc * sum_weight_calc) /
              sum(sum_weight_calc),
            stratum_catch_wt = sum(catch_weight)) |>
  ungroup() #|>
  #select(-sample_id)

Mean_weight_stratum <- Mean_wt_samples %>%
  group_by(year, grouping_code) %>%
  summarize(stratum_mean_wt =
              sum(mean_weight_calc * sum_weight_calc) /
              sum(sum_weight_calc),
            stratum_catch_wt = sum(catch_weight)) |>
  ungroup()

#Equivalent to Eq C.8 in the 2018 assessment
# Annual_mean_wt_weighted_with_sample_id <- Mean_weight_stratum_with_sample_id %>%
#   group_by(year) %>%
#   summarize(annual_mean_weight =
#               sum(stratum_mean_wt * stratum_catch_wt) /
#               sum(stratum_catch_wt))

Annual_mean_wt_weighted <- Mean_weight_stratum %>%
  group_by(year) %>%
  summarize(annual_mean_weight =
              sum(stratum_mean_wt * stratum_catch_wt) /
              sum(stratum_catch_wt))

# Now do an unweighted version
# Get the annual mean weight (unweighted by catch weight)
Annual_mean_wt_raw <- lengthwt_raw %>%
  group_by(year) %>%
  summarize(annual_mean_weight=mean(weight_calc))

# plot measured weight against calculated weight
# raw
g <- lengthwt_raw %>%
  filter(!is.na(weight)) %>%
  ggplot() +
  geom_point(aes(x=weight, y=weight_calc), colour="darkblue") +
  gfplot::theme_pbs() +
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  labs(title = paste(AREA), y = "Calculated weight from length", x = "Measured weight")
# print(g)

# Plot annual mean weights
# g <- Annual_mean_wt_raw %>%
#   rename(raw=annual_mean_weight) %>%
#   left_join(Annual_mean_wt_weighted) %>%
#   rename(weighted=annual_mean_weight) %>%
#   left_join(Annual_mean_wt_weighted_with_sample_id) %>%
#   rename(weighted_sample_id=annual_mean_weight) %>%
#   melt(id.vars="year", variable.name="measurement_type", value.name="mean_weight") %>%
#   ggplot()+
#   geom_line(aes(x=year, y=mean_weight, colour=measurement_type,
#                 linetype=measurement_type), size=1.5)+
#   ylim(0,2.5)+
#   gfplot::theme_pbs()+
#   theme(axis.text.x = element_text(size=12))+
#   theme(axis.text.y = element_text(size=12))+
#   theme(axis.title.x = element_text(size=14))+
#   theme(axis.title.y = element_text(size=14))+
#   theme(legend.text = element_text(size=12))+
#   theme(legend.title = element_text(size=13))+
#   labs(title = paste(AREA), y = "Mean weight", x = "Year")
# g

# Sean's code
# cmw <- readr::read_csv(here::here("data/generated/all-commercial-mean-weight.csv"))
# cmw <- dplyr::filter(cmw, area == AREA) %>%
#   rename(commercial_mw = mean_weight) %>%
#   select(-area)
#
# if (AREA == "5ABCD") {
#   cmw$year <- cmw$year
# }
#
# if (TYPE == "weighted") {
#   dat <- Annual_mean_wt_weighted %>%
#     rename(survey_mw = annual_mean_weight) %>%
#     full_join(cmw) %>%
#     arrange(year) %>%
#     filter(year >= 2000)
#   # # View(dat)
# } else {
#   dat <- Annual_mean_wt_raw %>%
#     rename(survey_mw = annual_mean_weight) %>%
#     full_join(cmw) %>%
#     arrange(year) %>%
#     filter(year >= 2000)
#   # View(dat)
# }
#
# if (AREA == "5ABCD") {
#   # dat <- filter(dat, year != 2007)
# }
#
# g1 <- tidyr::pivot_longer(dat, cols = 2:3) %>%
#   filter(!is.na(value)) %>%
#   ggplot(aes(year, value, colour = name)) +
#   geom_vline(xintercept = 2000:2021, lty = 1, col = "grey80") +
#   geom_point() +
#   geom_line() +
#   ggtitle(paste(AREA, TYPE))+
#   theme_light()
# print(g1)
#
# r <- range(log(c(dat$survey_mw, dat$commercial_mw)), na.rm = TRUE)
#
# g <- ggplot(dat, aes(log(survey_mw), log(commercial_mw))) +
#   geom_point() +
#   # stat_smooth(method = "lm", se = FALSE) +
#   stat_smooth(method=function(formula,data,weights=weight) MASS::rlm(formula,
#     data,
#     weights=weight,
#     method="MM"),
#     fullrange=TRUE, se = FALSE) +
#   ggrepel::geom_text_repel(aes(label = year), size = 4) +
#   geom_abline(intercept = 0, slope = 1) +
#   coord_fixed(xlim = c(r[1], r[2]), ylim = c(r[1], r[2])) +
#   ggtitle(paste(AREA, TYPE))+
#   theme_light()
#
# # print(g)
# gg <- cowplot::plot_grid(g1, g, nrow = 1, align = "hv")
# print(gg)
#
#
