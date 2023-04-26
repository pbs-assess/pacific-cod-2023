#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot locations of commercial length samples
# Robyn Forrest. July 6 2022.
# For Pacific cod TWG
library(tidyverse)
library(gfdata)
library(gfplot)
library(lubridate)
library(here)
library(PBSmapping)

## # Load the raw data
minyear <- 2008

my.dir <- here("data/pcod-cache")

# dat.file1 sent by Philina
# dat.file2 from
  #gfdata::cache_pbs_data(species = "pacific cod",
  #                     path = my.dir,
  #                     survey_sets = TRUE,
  #                     unsorted_only = FALSE,
  #                     return_all_lengths = TRUE)
dat.file1 <- file.path(my.dir,"all-pcod-samples.rds")
dat.file2 <- file.path(my.dir,"pacific-cod.rds")

d <- readRDS(dat.file1)
d2 <- readRDS(dat.file2)

unique(d$TRIP_SUB_TYPE_CODE)

# Look up area and gear names from gfplot queries
d_stat <- d2$catch %>%
  select(major_stat_area_code,major_stat_area_name) %>%
  distinct() %>%
  arrange(major_stat_area_code) %>%
  mutate(major_stat_area_code=as.double(major_stat_area_code))
d_stat

d_gear <- d2$commercial_samples %>%
  select(gear_code,gear_desc) %>%
  distinct() %>%
  arrange(gear_code)
d_gear

# process the table to make it a bit easier to deal with
d_samples <- d %>%
  select(TRIP_SUB_TYPE_CODE,SPECIES_CODE,SAMPLE_DATE,GEAR_CODE,VESSEL_ID, SAMPLE_ID,
         Best_Depth,Best_Lat,Best_Long,
         MAJOR_STAT_AREA_CODE,FISHING_EVENT_ID,TRIP_ID,CATCH_WEIGHT,N_Lengths) %>%
  mutate(year=lubridate::year(SAMPLE_DATE),
         month=lubridate::month(SAMPLE_DATE),
         MAJOR_STAT_AREA_CODE=as.double(MAJOR_STAT_AREA_CODE)) %>%
  filter(year>=minyear,
         !is.na(Best_Lat),
         !is.na(Best_Long),
         MAJOR_STAT_AREA_CODE %in% 3:9,
         GEAR_CODE %in% c(1,8),
         !TRIP_SUB_TYPE_CODE %in% 2:3) # copying gfdata sql code get-comm-samples.sql

# get samples by stock area
# 1. from the GFFOS table
d_samples_3cd1 <- d_samples %>%
  filter(MAJOR_STAT_AREA_CODE %in% 3:4)

d_samples_5abcd1 <- d_samples %>%
  filter(MAJOR_STAT_AREA_CODE %in% 5:8)

# 2. from gfdata d2$commercial samples (no lat-lons)
d_samples_3cd2 <- d2$commercial_samples %>%
  filter(year>=minyear,
         gear_desc %in% c("BOTTOM TRAWL", "UNKNOWN TRAWL"),
         major_stat_area_name %in% c("3C: S.W. VANCOUVER ISLAND",
                                     "3D: N.W. VANCOUVER ISLAND"))

d_samples_5abcd2 <- d2$commercial_samples %>%
  filter(year>=minyear,
         gear_desc %in% c("BOTTOM TRAWL", "UNKNOWN TRAWL"),
         major_stat_area_name %in% c("5A: SOUTHERN Q.C. SOUND" ,
                                     "5B: NORTHERN Q.C. SOUND",
                                     "5C: SOUTHERN HECATE STRAIT",
                                     "5D: NORTHERN HECATE STRAIT"))

# Summarize the data
# 3CD
summary_3cd1 <- d_samples_3cd1 %>%
    select(year,SAMPLE_ID,GEAR_CODE,VESSEL_ID, N_Lengths) %>%
    group_by(year,GEAR_CODE) %>%
    summarize(nsamples=n_distinct(SAMPLE_ID),
              nlengths=sum(N_Lengths),
              nvessels=n_distinct(VESSEL_ID))

summary_3cd2 <- d_samples_3cd2 %>%
  select(year,sample_id,gear_desc,vessel_id,length) %>%
  group_by(year,gear_desc) %>%
  summarize(nsamples=n_distinct(sample_id),
            nlengths=sum(!is.na(length)),
            nvessels=n_distinct(vessel_id))

  View(summary_3cd1)
  View(summary_3cd2)

  sample_id1 <- unique(d_samples_3cd1$SAMPLE_ID)
  sample_id2 <- unique(d_samples_3cd2$sample_id)

  N_sample_id1 <- length(sample_id1)
  N_sample_id2 <- length(sample_id2)
  N_sample_id1
  N_sample_id2

  FE_id1 <- sort(unique(d_samples_3cd1$FISHING_EVENT_ID))
  FE_id2 <- sort(unique(d_samples_3cd2$fishing_event_id))

  FEmatch <- match(FE_id1,FE_id2)

  # All match, but there are two extra samples in FE_id2

  # 5ABCD
  summary_5abcd1 <- d_samples_5abcd1 %>%
    select(year,SAMPLE_ID,GEAR_CODE,VESSEL_ID,N_Lengths) %>%
    group_by(year,GEAR_CODE) %>%
    summarize(nsamples=n_distinct(SAMPLE_ID),
              nlengths=sum(N_Lengths),
              nvessels=n_distinct(VESSEL_ID))

  summary_5abcd2 <- d_samples_5abcd2 %>%
    select(year,sample_id,gear_desc,vessel_id,length) %>%
    group_by(year,gear_desc) %>%
    summarize(nsamples=n_distinct(sample_id),
              nlengths=sum(!is.na(length)),
              nvessels=n_distinct(vessel_id))

  #View(summary_5abcd1)
  #View(summary_5abcd2)

  sample_id1 <- unique(d_samples_5abcd1$SAMPLE_ID)
  sample_id2 <- unique(d_samples_5abcd2$sample_id)

  N_sample_id1 <- length(sample_id1)
  N_sample_id2 <- length(sample_id2)
  N_sample_id1
  N_sample_id2

  FE_id1 <- sort(unique(d_samples_5abcd1$FISHING_EVENT_ID))
  FE_id2 <- sort(unique(d_samples_5abcd2$fishing_event_id))

  FEmatch <- match(FE_id1,FE_id2)
  FEmatch
  # More mismatches for 5ABCD. 10 more samples in all_samples



# Now map
source(here("R/plot-length-spatial.R"))

# 3CD
plot_comm_sets(d_samples_3cd1,
               min_year = 2017,
               bydate=TRUE,
               utm_zone = 9,
               bath = c(100,200, 500),
               xlim = c(500, 890), ylim = c(5350, 5650))
ggsave(here("report/figures/commercial_sample_locations_bydate_3CD.png"))

plot_comm_sets(d_samples_3cd1,
               min_year = 2010,
               bydate=FALSE,
               utm_zone = 9,
               bath = c(100,200, 500),
               xlim = c(500, 890), ylim = c(5350, 5650))
ggsave(here("report/figures/commercial_sample_locations_byyear_3CD.png"))

# 5ABCD
plot_comm_sets(d_samples_5abcd1,
               min_year = 2018,
               bydate=TRUE,
               utm_zone = 9,
               bath = c(100,200, 500),
               xlim = c(200, 700), ylim = c(5600, 6100))
ggsave(here("report/figures/commercial_sample_locations_bydate_5ABCD.png"))

plot_comm_sets(d_samples_5abcd1,
               min_year = 2011,
               bydate=FALSE,
               utm_zone = 9,
               bath = c(100,200, 500),
               xlim = c(200, 700), ylim = c(5600, 6100))
ggsave(here("report/figures/commercial_sample_locations_byyear_5ABCD.png"))


##########################################################
# OLD approach not working because dat$catch and dat$commercial_samples
#   do not share fishing_event_id or trip_id
#
#   dat.file <- here("data/pcod-cache/pacific-cod.rds")
#
#   if(!file.exists(dat.file)){
#     gfdata::cache_pbs_data(species = "pacific cod",
#                            path = here("data/pcod-cache"),
#                            survey_sets = TRUE,
#                            unsorted_only = FALSE,
#                            return_all_lengths = TRUE)
#
#   }
#   dat <- readRDS(dat.file)
#
# # First, get a summary of commercial length sampling by year
# # Just look at WCVI for now
# d_samples1 <- dat$commercial_samples %>%
#   dplyr::filter(major_stat_area_name %in%
#                   c("3C: S.W. VANCOUVER ISLAND","3D: N.W. VANCOUVER ISLAND"),
#                 year>minyear)
#
# summary_by_year <- d_samples1 %>%
#   select(year,sample_id,gear_desc,length) %>%
#   group_by(year,gear_desc) %>%
#   summarize(nsamples=n_distinct(sample_id),
#             nlengths=sum(!is.na(length)),
#             meanlength=round(mean(length),2),
#             sdlength=round(sd(length),2),
#             selength=round(sdlength/sqrt(nlengths),2))
#
# colnames(summary_by_year) <- c("Year", "Gear","Num samples", "Num lengths","Raw mean length", "SD length", "SE length")
#
# summary_by_year
#
# # OK so there are commercial samples for every year since 2010, except 2018
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Now try to plot them on a map
# # Get length samples with locations
# # dat$commercial_samples does not include lats and longs,
# #  so join with dat$catch
# # try joining on fishing_event_id
# areas <- c("5B: NORTHERN Q.C. SOUND",
#            "5C: SOUTHERN HECATE STRAIT" ,
#            "5D: NORTHERN HECATE STRAIT",
#            "5A: SOUTHERN Q.C. SOUND",
#            "3C: S.W. VANCOUVER ISLAND",
#            "3D: N.W. VANCOUVER ISLAND")
#
# d_samples2 <- dat$commercial_samples %>%
#   dplyr::filter(major_stat_area_name %in% areas,
#                 year>minyear,
#                 gear_desc %in% c("GROUNDFISH TRAWL", "UNKNOWN TRAWL"))
#
# # fishing_event_id and trip_id are character vectors in this table (why?)
# # But looks like it happens in this SQL
# # https://github.com/pbs-assess/gfdata/blob/master/inst/sql/get-catch.sql
# d_catch <- dat$catch %>%
#   dplyr::filter(year>=minyear,
#                 major_stat_area_name %in% areas,
#                 gear %in% c("GROUNDFISH TRAWL", "UNKNOWN TRAWL")) %>%
#   mutate(fishing_event_id = as.double(fishing_event_id),
#          trip_id = as.double(trip_id))
#
# # left joining the above two tables doesn't work because no
# #   common fishing_event_ids. All fields from d_samples2 are NA
# d_samps_loc <- d_catch %>%
#   left_join(d_samples2,
#             by="trip_id")%>%
#   select(year.x,year.y, best_date,trip_id, vessel_name,
#          lat, lon, sample_id,length) %>%
#   filter(!is.na(lat),!is.na(lon),!is.na(length))
#
# # all sample ids and lengths are NA
# View(d_samps_loc)
#
# # there are no common fishing_event_ids or trip ids
# fevents <- unique(d_samples2$fishing_event_id)
# tripids <- unique(d_samples2$trip_id)
#
# fe_test <- d_catch %>%
#   filter(fishing_event_id %in% fevents) %>%
#   nrow()
#
# trip_test <- d_catch %>%
#   filter(trip_id %in% tripids) %>%
#   nrow()
#
# # Philina's code also shows that fishing_event_ids are not the same in the two tables
# d_samples2 <- dat$commercial_samples %>%
#   dplyr::filter(year>minyear)
#
# #d_catch <- dat$cpue_spatial %>%
# d_catch <- dat$catch %>%
#   dplyr::filter(year>=minyear) %>%
#   mutate(fishing_event_id = as.double(fishing_event_id),
#          trip_id = as.double(trip_id))
#
# d <- d_samples2 %>%
#   left_join(d_catch, by=c("fishing_event_id"))#%>%
#
# d <- d %>%
#   select(trip_start_date, best_date, fishing_event_id, vessel_id,
#          vessel_registration_number,
#          trip_id.x, trip_id.y,gear_desc,
#          trip_sub_type_desc, catch_weight, lat, lon) %>%
#   distinct()
#
