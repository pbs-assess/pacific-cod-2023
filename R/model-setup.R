## -----------------------------------------------------------------------------
## Set verbosity for this project (R code)
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
last.assess.yr <- 2018
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
end.yr <- 2020
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2020
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
base.model.3cd.name <- ifelse(french, "Sc 1a. Ref", "2020 Reference 3CD")
base.model.3cd.dir.name <- file.path(model.dir,
                                     "1_1a_3CD_BASE_2020")

base.model.5abcd.name <- ifelse(french, "Sc 1a. Ref", "2020 Reference 5ABCD")
base.model.5abcd.dir.name <- file.path(model.dir,
                                       "0_1a_5ABCD_BASE_2020")

if(verbose){
  cat0("Base model directory name for reference model 5abcd:\n", base.model.5abcd.dir.name)
  cat0("Base model pretty name for reference model 5abcd:\n", base.model.5abcd.name)
  cat0("Base model directory name for reference model 3cd:\n", base.model.3cd.dir.name)
  cat0("Base model pretty name for reference model 3cd:\n", base.model.3cd.name)
}


## -----------------------------------------------------------------------------
## Sensitivity models group 1 (5ABCD)
## -----------------------------------------------------------------------------
sens.models.dir.name.1 <- c(file.path(model.dir,
                                     "0_1b_5ABCD_SurveyMnWt_interp"),
                            file.path(model.dir,
                                      "0_1c_5ABCD_SurveyMnWt_no_interp"))

sens.models.name.1 <- c("Scenario 1",
                        "Scenario 2")

## -----------------------------------------------------------------------------
## Sensitivity models group 11 (3CD) - individual imputation iterations
## -----------------------------------------------------------------------------
sens.models.dir.name.11 <- c(file.path(model.dir,
                                       "1_1b_3CD_SurveyMnWt_interp"),
                             file.path(model.dir,
                                       "1_1c_3CD_SurveyMnWt_no_interp"))

sens.models.name.11 <- c("Scenario 1",
                         "Scenario 2")


## -----------------------------------------------------------------------------

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model.5abcd <<- load.models(base.model.5abcd.dir.name)
  sens.models.1 <<- load.models(sens.models.dir.name.1)
  base.model.3cd <<- load.models(base.model.3cd.dir.name)
  sens.models.11 <<- load.models(sens.models.dir.name.11)
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
  create.rdata.file(base.model.5abcd.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    burnin = burnin,
                    thin = thin,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)
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
  sens.models.names.list <- c( unlist(sens.models.dir.name.1),
                               unlist(sens.models.dir.name.11))
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
}
