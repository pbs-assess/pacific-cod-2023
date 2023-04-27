# Pacific Cod 2023
# Robyn Forrest, Pacific Biological Station, April 29, 2023

# Pull raw data for Pacific Cod stocks from GFBio using gfdata package
# 1. This can be run as standalone if preparing the data files for the models OR
# 2. it is also called by all.R for making the report.
# Note that if (2) the file data/pcod-cache/pacific-cod.rds should be the same as that used
#   in the model data files. As long as there is a pacific-cod.rds file in the pcod-cache folder
#   this code will not pull the data again
# Note, must be on the DFO network to pull the data

## # Load the raw data

# Note that return_all_lengths must be set to TRUE because some lengths since
# 2016 were recorded as TOTAL_LENGTH instead of FORK_LENGTH.
# Maria Cornthwaite confirmed this would have been a recording error rather
#    than measurement error (May 2022)
# UPDATE: April 29 2023: Looks like this has been fixed

dat.file <- file.path(rootd.data,
                      "pcod-cache",
                      "pacific-cod.rds")

if(!dir.exists(file.path(rootd.data,"pcod-cache"))) dir.create(file.path(rootd.data,"pcod-cache"))

if(!file.exists(dat.file)){
  gfdata::cache_pbs_data(species = "pacific cod",
                 path = file.path(rootd.data,
                                  "pcod-cache"),
                 survey_sets = TRUE,
                 unsorted_only = FALSE,
                 return_all_lengths = TRUE)
}
dat <- readRDS(dat.file)

# Also run get_survey_sets (needed for some analyes)
surv.dat.file <- file.path(rootd.data,
                           "pcod-cache",
                           "pacific-cod-survey-sets.rds")
if(!file.exists(surv.dat.file)){
  survdat <- gfdata::get_survey_sets(species="pacific cod",
                                     join_sample_ids = TRUE)

  saveRDS(survdat, file.path(rootd.data,
                             "pcod-cache",
                             "pacific-cod-survey-sets.rds"))
}
survdat <- readRDS(surv.dat.file)
