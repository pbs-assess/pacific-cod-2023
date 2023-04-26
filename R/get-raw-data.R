# Pull raw data for Pacific Cod stocks from GFBio using gfdata package
# This is sourced from all.R

## # Load the raw data
dat.file <- file.path(rootd.data,
                      "pcod-cache",
                      "pacific-cod.rds")

if(!dir.exists(file.path(rootd.data,"pcod-cache"))) dir.create(file.path(rootd.data,"pcod-cache"))

if(!file.exists(dat.file)){
  gfdata::cache_pbs_data(species = "pacific cod",
                 path = file.path(rootd.data,
                                  "pcod-cache"),
                 survey_sets = TRUE,
                 unsorted_only = FALSE)
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
