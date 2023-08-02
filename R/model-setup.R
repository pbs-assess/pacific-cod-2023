## -----------------------------------------------------------------------------
## Set verbosity for this project (R code)
## NOTE THAT ALL THE TWG FOLDERS HAVE BEEN MOVED INTO MODELS/TWG
## TO BUILD ANY OF THE TWG REPORTS, THEY NEED TO BE MOVED BACK INTO MODELS
## AND THE BASE MODEL NEEDS TO BE CHANGED BELOW
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.025, 0.975)

## -----------------------------------------------------------------------------
## iscam files with names that don't change depending on model
rep.file <- "iscam.rep"
par.file <- "iscam.par"
mcmc.file <- "iscam_mcmc.csv"
mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
mcmc.recr.file <- "iscam_rt_mcmc.csv"
mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
mcmc.natural.mort.file <- "iscam_m_mcmc.csv"
mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
mpd.proj.file <- "iscammpd_proj_Gear1.csv"
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- as.numeric(substr(Sys.Date(), 1, 4))
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- 2022
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## rootd.models all.r
## -----------------------------------------------------------------------------
model.dir <- rootd.models
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
exe.file.name <- "iscam.exe"
if(verbose) cat0("iscam executable file: \n  ", exe.file.name)
starter.file.name <- "iscam.dat"
if(verbose) cat0("iscam starter file: \n  ", starter.file.name)

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Start year for the models
start.yr <- 1956
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}

## Start year for the fishery age comps
## start.yr.age.comps <- 1951
## if(verbose){
##   cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
## }

## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2022
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2022
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
# THIS ONE IS FOR THE TWG REPORTS
# base.model.3cd.dir.name <- file.path(model.dir,
#                                      "1a_3CD_2023_interp")
# base.model.3cd.name <- ifelse(french, "Sc 1a. Ref", "Sc. 1 GLM Interpolation")

# THIS ONE IS FOR THE SCIENCE RESPONSE
base.model.3cd.dir.name <- file.path(model.dir,
                                     "1a_3CD_2023_reference")
base.model.3cd.name <- ifelse(french, "Sc 1a. Ref", "Sc. 1a Reference")

if(verbose){
  cat0("Base model directory name for reference model 3cd:\n", base.model.3cd.dir.name)
  cat0("Base model pretty name for reference model 3cd:\n", base.model.3cd.name)
}

## --------------------------------------------------------------------------------------------
## Sensitivity models for the TWG REPORTS 2023. SEE NOTE AT THE TOP OF THIS
##  SCRIPT ABOUT LOCATION OF THESE FOLDERS. THEY SHOULD BE MOVED BACK INTO MODELS IF NEEDED.
## --------------------------------------------------------------------------------------------
# Sensitivity of treatment of commercial mean weight index
# sens.models.dir.name.11 <- c(file.path(model.dir,
#                                        "1b_3CD_2023_no_interp"),
#                              file.path(model.dir,
#                                        "1c_3CD_2023_noGLM_extrap"),
#                              file.path(model.dir,
#                                        "1d_3CD_2023_noGLM_no_extrap")
#                              )
#
# sens.models.name.11 <- c("Sc. 2 GLM No interpolation",
#                          "Sc. 3 No GLM, with extrap",
#                          "Sc. 4 No GLM, no extrap")
#
# # New comparisons
# # Include the 2017 mean weight index point
# sens.models.dir.name.22 <- file.path(model.dir,
#                                        "1a_3CD_2023_interp_incl_2017")
#
# sens.models.name.22 <- "Sc. 1a GLM Interpolation (incl. 2017)"
#
# # Update the length-weight parameters
# sens.models.dir.name.33 <- file.path(model.dir,
#                                      "1a_3CD_2023_interp_new_lw")
#
# sens.models.name.33 <- "Sc. 1d GLM Interpolation (new lw pars)"
#
# # Reduce sig_w
# sens.models.dir.name.44 <- c(file.path(model.dir,
#                                      "1a_3CD_2023_interp_2017_sigw015"),
#                              file.path(model.dir,
#                                        "1a_3CD_2023_interp_2017_sigw01"))
#
#
# sens.models.name.44 <- c("Sc. 1b GLM Interpolation (sigW=0.15)",
#                          "Sc. 1c GLM Interpolation (sigW=0.1)")
#
# # Sensitivity to 2019 index point
# sens.models.dir.name.55 <- c(file.path(model.dir,
#                                        "1a_3CD_2023_interp_no2019"),
#                              file.path(model.dir,
#                                        "1a_3CD_2023_interp_no2019b"),
#                              file.path(model.dir,
#                                        "1a_3CD_2023_interp_old2019"))
#
# sens.models.name.55 <- c("Sc. 1e GLM (no 2019)",
#                          "Sc. 1f GLM (no 2018-2019)",
#                          "Sc. 1g GLM (no 2018, old 2019)")
#
## -----------------------------------------------------------------------------
## Decision table models to average (3CD)
## -----------------------------------------------------------------------------
desc.models.3cd.dir.name <- c(base.model.3cd.dir.name,
                              file.path(model.dir,
                                        "2d_3CD_q_1"),
                              file.path(model.dir,
                                        "2e_3CD_q_cv06"),
                              file.path(model.dir,
                                        "3a_3CD_Mprior_mean04_sd01"),
                              file.path(model.dir,
                                        "5a_3CD_kage3"),
                              file.path(model.dir,
                                        "6b_3CD_sig015"),
                              file.path(model.dir,
                                        "7b_3CD_sigW015"))

desc.models.3cd.name <- c(base.model.3cd.name,
                          ifelse(french, "Sc 2d.", "2d) WCVISS ln(q) prior mean = ln(1.0)"),
                          ifelse(french, "Sc 2e.", "2e) WCVISS ln(q) prior SD = 0.6"),
                          ifelse(french, "Sc 3a.", "3a) M prior mean = 0.4, SD = 0.1"),
                          ifelse(french, "Sc 5a.", "5a) kage = 3y and update FW parameters"),
                          ifelse(french, "Sc 6b.", "6b) Fix sigma O = 0.15"),
                          ifelse(french, "Sc 7b.", "7b) Fix sigma W = 0.15"))

# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 13 sub (3CD) Decision table figure
# ## -----------------------------------------------------------------------------
sens.models.dir.name.13.sub <- c(file.path(model.dir,
                                           "2d_3CD_q_1"),
                                 file.path(model.dir,
                                           "2e_3CD_q_cv06"),
                                 file.path(model.dir,
                                           "3a_3CD_Mprior_mean04_sd01"),
                                 file.path(model.dir,
                                           "5a_3CD_kage3"),
                                 file.path(model.dir,
                                           "6b_3CD_sig015"),
                                 file.path(model.dir,
                                           "7b_3CD_sigW015"))

sens.models.name.13.sub <- c("2d) WCVISS ln(q) prior mean = ln(1.0)",
                             "2e) WCVISS ln(q) prior SD = 0.6",
                             "3a) M prior mean = 0.4, SD = 0.1",
                             "5a) kage = 3y and update FW parameters",
                             "6b) Fix sigma O = 0.15",
                             "7b) Fix sigma W = 0.15")

## -----------------------------------------------------------------------------
## Retrospectives
## -----------------------------------------------------------------------------
retro.dir.names.3cd <- c(file.path(base.model.3cd.dir.name,
                                   "Retrospective01"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective02"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective03"),
                         file.path(base.model.3cd.dir.name,
                                   "Retrospective04"))

retro.dir.names <- c(retro.dir.names.3cd)

retro.names <- c("- 1 year",
                 "- 2 years",
                 "- 3 years",
                 "- 4 years")

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above.
load.models.into.parent.env <- function(){
  base.model.3cd     <<- load.models(base.model.3cd.dir.name)
  sens.models.13.sub <<- load.models(sens.models.dir.name.13.sub)
  #sens.models.11    <<- load.models(sens.models.dir.name.11)
  #sens.models.22    <<- load.models(sens.models.dir.name.22)
  #sens.models.33    <<- load.models(sens.models.dir.name.33)
  #sens.models.44    <<- load.models(sens.models.dir.name.44)
  #sens.models.55    <<- load.models(sens.models.dir.name.55)
  desc.models.3cd    <<- load.models(desc.models.3cd.dir.name)
  avg.model.3cd      <<- avg.models(desc.models.3cd)

  # base.retro.models.3cd <<- load.models(retro.dir.names.3cd)
}

build <- function(ovwrt.base = FALSE,
                  ovwrt.sens = FALSE,
                  ovwrt.retro = FALSE,
                  burnin = 1000,
                  thin = 1){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.
  ##
  ## ovwrt.base - overwrite the RData file for the base model?
  ## ovwrt.sens - overwrite the RData files for the sensitivity models?
  ## ovwrt.retro - overwrite the RData files for the retrospective models?

  ## Base models
  create.rdata.file(base.model.3cd.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    burnin = burnin,
                    thin = thin,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the for loop below to work right
  sens.models.names.list <-   c(unlist(sens.models.dir.name.13.sub),
                                unlist(desc.models.3cd.dir.name))
  #                             unlist(sens.models.dir.name.11),
  #                             unlist(sens.models.dir.name.22),
  #                             unlist(sens.models.dir.name.33),
  #                             unlist(sens.models.dir.name.44),
  #                             unlist(sens.models.dir.name.55))


  ## Sensitivity models
  for(model.nm in sens.models.names.list){
    create.rdata.file(model.nm,
                      ovwrt.rdata = ovwrt.sens,
                      load.proj = TRUE,
                      burnin = burnin,
                      thin = thin,
                      low = confidence.vals[1],
                      high = confidence.vals[2],
                      verbose = verbose)
  }
  #
  # ## Retrospective models
  # for(model.nm in retro.dir.names){
  #   create.rdata.file(model.nm,
  #                     ovwrt.rdata = ovwrt.retro,
  #                     load.proj = TRUE,
  #                     burnin = burnin,
  #                     thin = thin,
  #                     low = confidence.vals[1],
  #                     high = confidence.vals[2],
  #                     verbose = verbose)
  # }
}

