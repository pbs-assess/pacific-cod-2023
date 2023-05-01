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
last.assess.yr <- 2021
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
base.model.3cd.name <- ifelse(french, "Sc 1a. Ref", "1a) Reference model 3CD")
base.model.3cd.dir.name <- file.path(model.dir,
                                     "1_1a_3CD_BASE_2023")

# base.model.5abcd.name <- ifelse(french, "Sc 1a. Ref", "1a) Reference model 5ABCD")
# base.model.5abcd.dir.name <- file.path(model.dir,
#                                       "0_1a_5ABCD_BASE_2023")

if(verbose){
  # cat0("Base model directory name for reference model 5abcd:\n", base.model.5abcd.dir.name)
  # cat0("Base model pretty name for reference model 5abcd:\n", base.model.5abcd.name)
  cat0("Base model directory name for reference model 3cd:\n", base.model.3cd.dir.name)
  cat0("Base model pretty name for reference model 3cd:\n", base.model.3cd.name)
}

## -----------------------------------------------------------------------------
## Sensitivity models group 11 (3CD) - individual imputation iterations
## -----------------------------------------------------------------------------
sens.models.dir.name.11 <- c(file.path(model.dir,
                                       "1_1b_3CD_BASE_2023__no_interp"))

sens.models.name.11 <- c("Sc. 2 no interpolation")

## -----------------------------------------------------------------------------
## Decision table models to average (5ABCD)
## -----------------------------------------------------------------------------
# desc.models.5abcd.dir.name <- c(base.model.5abcd.dir.name,
#                                 file.path(model.dir,
#                                           "0_2d_5ABCD_rsoleq_1_1"),
#                                 file.path(model.dir,
#                                           "0_2e_5ABCD_q_cv06"),
#                                 file.path(model.dir,
#                                           "0_3a_5ABCD_Mprior_mean04_sd01"),
#                                 file.path(model.dir,
#                                           "0_5a_5ABCD_kage_3"),
#                                 file.path(model.dir,
#                                           "0_6b_5ABCD_sig015"),
#                                 file.path(model.dir,
#                                           "0_7b_5ABCD_sigW_015"))
# desc.models.5abcd.name <- c(base.model.5abcd.name,
#                             ifelse(french, "Sc 2d.", "2d) HSSS ln(q) prior mean = ln(1.0 * 0.35), QCSSS = ln(1.0 * 0.65)"),
#                             ifelse(french, "Sc 2e.", "2e) HSSS and QCSS ln(q) prior SD = 0.6"),
#                             ifelse(french, "Sc 3a.", "3a) M prior mean = 0.4, SD = 0.1"),
#                             ifelse(french, "Sc 5a.", "5a) kage = 3y and update FW parameters"),
#                             ifelse(french, "Sc 6b.", "6b) Fix sigma O = 0.15"),
#                             ifelse(french, "Sc 7b.", "7b) Fix sigma W = 0.15"))
#

## MOST OF THESE SENSITIVITY MODELS ARE NOT USED -- DELETE FOR NOW
## -----------------------------------------------------------------------------
## Sensitivity models group 0 (5ABCD)
## -----------------------------------------------------------------------------
# sens.models.dir.name.0 <- c(file.path(model.dir,
#                                       "0_0a_5ABCD_BASE-preReview"),
#                             file.path(model.dir,
#                                       "0_0b_5ABCD_BASE-UpdateGrowth"))
#
# sens.models.name.0 <- c("Female growth parameters, no 2018 Catch",
#                         "2-sex growth parameters, no 2018 Catch")

## -----------------------------------------------------------------------------
## Sensitivity models group 1 subset (5ABCD)
## -----------------------------------------------------------------------------
# sens.models.dir.name.1.sub <- c(file.path(model.dir,
#                                           "0_1b_5ABCD_NO_loc-yr-interact_rsoleq_06_019"),
#                                 file.path(model.dir,
#                                           "0_1c_5ABCD_NO_loc-yr-interact_rsoleq_06_019_doubleCV"))
#
# sens.models.name.1.sub <- c("1b) No locality/yr interaction",
#                             "1c) No locality/yr interaction, double CV")

## -----------------------------------------------------------------------------
## Sensitivity models group 1 subset 2 (5ABCD)
## -----------------------------------------------------------------------------
# sens.models.dir.name.1.sub2 <- c(file.path(model.dir,
#                                            "0_1b_5ABCD_NO_loc-yr-interact_rsoleq_06_019"),
#                                  file.path(model.dir,
#                                            "0_1c_5ABCD_NO_loc-yr-interact_rsoleq_06_019_doubleCV"),
#                                  file.path(model.dir,
#                                            "0_1d_5ABCD_loc-yr-interact_no_recentCPUE"))
#
# sens.models.name.1.sub2 <- c("1b) No locality/yr interaction",
#                              "1c) No locality/yr interaction, double CV",
#                              "1d) Remove post-1995 CPUE")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 1 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.1 <- c(file.path(model.dir,
#                                       "0_1b_5ABCD_NO_loc-yr-interact_rsoleq_06_019"),
#                             file.path(model.dir,
#                                       "0_1c_5ABCD_NO_loc-yr-interact_rsoleq_06_019_doubleCV"),
#                             file.path(model.dir,
#                                       "0_1d_5ABCD_loc-yr-interact_no_recentCPUE"),
#                             file.path(model.dir,
#                                       "0_1e_5ABCD_BASE_loc-yr-interact_no_histCPUE"),
#                             file.path(model.dir,
#                                       "0_1f_5ABCD_BASE_loc-yr-interact_no_CPUE"))
#
#
# sens.models.name.1 <- c("1b) No locality/yr",
#                         "1c) No locality/yr double CV",
#                         "1d) Remove post-1995 CPUE",
#                         "1e) Remove pre-1996 CPUE",
#                         "1f) Remove all CPUE")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 2 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.2 <- c(file.path(model.dir,
#                                       "0_2a_5ABCD_rsoleq_019_019"),
#                             file.path(model.dir,
#                                       "0_2b_5ABCD_q_all_unif"),
#                             file.path(model.dir,
#                                       "0_2c_5ABCD_rsoleq_05_05"),
#                             file.path(model.dir,
#                                       "0_2d_5ABCD_rsoleq_1_1"),
#                             file.path(model.dir,
#                                       "0_2e_5ABCD_q_cv06"),
#                             file.path(model.dir,
#                                       "0_2f_5ABCD_q_cv1"))
# sens.models.name.2 <- c("2a) HSSS and QCSS ln(q) prior mean = ln(0.1869 * 0.65)",
#                         "2b) No priors on ln(q)",
#                         "2c) ln(q) prior mean HSSS  = ln(0.5 * 0.35), QCSSS = ln(0.5 * 0.65)",
#                         "2d) ln(q) prior mean HSSS = ln(1.0 * 0.35), QCSSS = ln(1.0 * 0.65)",
#                         "2e) HSSS and QCSS ln(q) prior SD = 0.6",
#                         "2f) HSSS and QCSS ln(q) prior SD = 1.0")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 2 (5ABCD) - sub
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.2.sub <- c(file.path(model.dir,
#                                       "0_2a_5ABCD_rsoleq_019_019"),
#                             file.path(model.dir,
#                                       "0_2c_5ABCD_rsoleq_05_05"),
#                             file.path(model.dir,
#                                       "0_2d_5ABCD_rsoleq_1_1"),
#                             file.path(model.dir,
#                                       "0_2e_5ABCD_q_cv06"),
#                             file.path(model.dir,
#                                       "0_2f_5ABCD_q_cv1"))
# sens.models.name.2.sub <- c("2a) HSSS and QCSS ln(q) prior mean = ln(0.1869 * 0.65)",
#                             "2c) ln(q) prior mean HSSS  = ln(0.5 * 0.35), QCSSS = ln(0.5 * 0.65)",
#                             "2d) ln(q) prior mean HSSS = ln(1.0 * 0.35), QCSSS = ln(1.0 * 0.65)",
#                         "2e) HSSS and QCSS ln(q) prior SD = 0.6",
#                         "2f) HSSS and QCSS ln(q) prior SD = 1.0")
#
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 3 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.3 <- c(file.path(model.dir,
#                                       "0_3a_5ABCD_Mprior_mean04_sd01"),
#                             file.path(model.dir,
#                                       "0_3b_5ABCD_Mprior_mean04_sd025"),
#                             file.path(model.dir,
#                                       "0_3c_5ABCD_Mprior_mean05_sd025"))
# sens.models.name.3 <- c("3a) M prior mean = 0.4, SD = 0.1",
#                         "3b) M prior mean = 0.4, SD = 0.25",
#                         "3c) M prior mean = 0.5, SD = 0.25")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 4 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.4 <- c(file.path(model.dir,
#                                       "0_4a_5ABCD_hprior_unif"),
#                             file.path(model.dir,
#                                       "0_4b_5ABCD_hprior_085"))
# sens.models.name.4 <- c("4a) Steepness uniform prior",
#                         "4b) Steepness beta prior mean=0.85, SD=0.75")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 5 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.5 <- c(file.path(model.dir,
#                                       "0_5a_5ABCD_kage_3"),
#                             file.path(model.dir,
#                                       "0_5b_5ABCD_old_growth_pars"))
# sens.models.name.5 <- c("5a) kage = 3y and update FW parameters",
#                         "5b) Use previous vonB pars")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 6 sub (5ABCD) #For decision table graph
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.6.sub <- c(file.path(model.dir,
#                                           "0_2d_5ABCD_rsoleq_1_1"),
#                                 file.path(model.dir,
#                                           "0_2e_5ABCD_q_cv06"),
#                                 file.path(model.dir,
#                                           "0_3a_5ABCD_Mprior_mean04_sd01"),
#                                 file.path(model.dir,
#                                           "0_5a_5ABCD_kage_3"),
#                                 file.path(model.dir,
#                                           "0_6b_5ABCD_sig015"),
#                                 file.path(model.dir,
#                                           "0_7b_5ABCD_sigW_015"))
#
# sens.models.name.6.sub <- c("2d) HSSS ln(q) prior mean = ln(1.0 * 0.35), QCSSS = ln(1.0 * 0.65)",
#                             "2e) HSSS and QCSS ln(q) prior SD = 0.6",
#                             "3a) M prior mean = 0.4, SD = 0.1",
#                             "5a) kage = 3y and update FW parameters",
#                             "6b) Fix sigma O = 0.15",
#                             "7b) Fix sigma W = 0.15"
#                             )
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 6 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.6 <- c(file.path(model.dir,
#                                       "0_6a_5ABCD_sig01"),
#                             file.path(model.dir,
#                                       "0_6b_5ABCD_sig015"),
#                             file.path(model.dir,
#                                       "0_6c_5ABCD_sig025"),
#                             file.path(model.dir,
#                                       "0_6d_5ABCD_tau1"))
# sens.models.name.6 <- c("6a) Fix sigma O = 0.1",
#                         "6b) Fix sigma O = 0.15",
#                         "6c) Fix sigma O = 0.25",
#                         "6d) Fix sigma R = 1.0")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 7 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.7 <- c(file.path(model.dir,
#                                       "0_7a_5ABCD_sigW_04"),
#                             file.path(model.dir,
#                                       "0_7b_5ABCD_sigW_015"),
#                             file.path(model.dir,
#                                       "0_7c_5ABCD_noHistMeanWt"))
# sens.models.name.7 <- c("7a) Fix sigma W = 0.4",
#                         "7b) Fix sigma W = 0.15",
#                         "7c) Remove historical mean weight data")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 108 (5ABCD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.108 <- c(file.path(model.dir,
#                                       "0_8a_5ABCD_InflateHistCatch_025"),
#                             file.path(model.dir,
#                                       "0_8b_5ABCD_InflateHistCatch_05"))
# sens.models.name.108 <- c("8a) Inflate pre-1996 catch by 25%",
#                         "8b) Inflate pre-1996 catch by 50%")
#

## -----------------------------------------------------------------------------
## Decision table models to average (3CD)
## -----------------------------------------------------------------------------
# desc.models.3cd.dir.name <- c(base.model.3cd.dir.name,
#                               file.path(model.dir,
#                                         "1_2d_3CD_q_1"),
#                               file.path(model.dir,
#                                         "1_2e_3CD_q_cv06"),
#                               file.path(model.dir,
#                                         "1_3a_3CD_Mprior_mean04_sd01"),
#                               file.path(model.dir,
#                                         "1_5a_3CD_kage3"),
#                               file.path(model.dir,
#                                         "1_6b_3CD_sig015"),
#                               file.path(model.dir,
#                                         "1_7b_3CD_sigW015"))
#
# desc.models.3cd.name <- c(base.model.3cd.name,
#                           ifelse(french, "Sc 2d.", "2d) WCVISS ln(q) prior mean = ln(1.0)"),
#                           ifelse(french, "Sc 2e.", "2e) WCVISS ln(q) prior SD = 0.6"),
#                           ifelse(french, "Sc 3a.", "3a) M prior mean = 0.4, SD = 0.1"),
#                           ifelse(french, "Sc 5a.", "5a) kage = 3y and update FW parameters"),
#                           ifelse(french, "Sc 6b.", "6b) Fix sigma O = 0.15"),
#                           ifelse(french, "Sc 7b.", "7b) Fix sigma W = 0.15"))

## -----------------------------------------------------------------------------
## Sensitivity models group 00 (3CD)
## -----------------------------------------------------------------------------
# sens.models.dir.name.00 <- c(file.path(model.dir,
#                                       "1_0a_3CD_BASE-preReview"),
#                             file.path(model.dir,
#                                       "1_0b_3CD_BASE-UpdateGrowth"))
#
# sens.models.name.00 <- c("Female growth parameters, no 2018 Catch",
#                          "2-sex growth parameters, no 2018 Catch")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 8 subset (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.8.sub <- c(file.path(model.dir,
#                                           "1_1b_3CD_NO_loc-yr-interact_rsoleq_0228sd03"),
#                                 file.path(model.dir,
#                                           "1_1c_3CD_NO_loc-yr-interact_rsoleq_0228sd03_doubleCV"))
#
# sens.models.name.8.sub <- c("1b) No locality/yr",
#                             "1c) No locality/yr, double CV")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 8 subset 2 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.8.sub2 <- c(file.path(model.dir,
#                                            "1_1b_3CD_NO_loc-yr-interact_rsoleq_0228sd03"),
#                                  file.path(model.dir,
#                                            "1_1c_3CD_NO_loc-yr-interact_rsoleq_0228sd03_doubleCV"),
#                                  file.path(model.dir,
#                                            "1_1d_3CD_no_recent_CPUE"),
#                                  file.path(model.dir,
#                                            "1_1g_3CD_no_triennial"))
#
# sens.models.name.8.sub2 <- c("1b) No locality/yr",
#                              "1c) No locality/yr, double CV",
#                              "1d) Remove post-1995 CPUE",
#                              "1g) Remove Triennial survey")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 8 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.8 <- c(file.path(model.dir,
#                                       "1_1b_3CD_NO_loc-yr-interact_rsoleq_0228sd03"),
#                             file.path(model.dir,
#                                       "1_1c_3CD_NO_loc-yr-interact_rsoleq_0228sd03_doubleCV"),
#                             file.path(model.dir,
#                                       "1_1d_3CD_no_recent_CPUE"),
#                             file.path(model.dir,
#                                       "1_1e_3CD_BASE_loc-yr-interact_no_histCPUE"),
#                             file.path(model.dir,
#                                       "1_1f_3CD_BASE_loc-yr-interact_no_CPUE"),
#                             file.path(model.dir,
#                                       "1_1g_3CD_no_triennial"))
#
# sens.models.name.8 <- c("1b) No locality/yr ",
#                         "1c) No locality/yr , double CV",
#                         "1d) Remove post-1995 CPUE",
#                         "1e) Remove pre-1996 CPUE",
#                         "1f) Remove all CPUE",
#                         "1g) Remove Triennial survey")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 9 sub (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.9.sub <- c(file.path(model.dir,
#                                           "1_2a_3CD_rsoleq_0228sd0448"),
#                                 file.path(model.dir,
#                                           "1_2c_3CD_q_05"),
#                                 file.path(model.dir,
#                                           "1_2d_3CD_q_1"),
#                                 file.path(model.dir,
#                                           "1_2e_3CD_q_cv06"),
#                                 file.path(model.dir,
#                                           "1_2f_3CD_q_cv1"))
#
# sens.models.name.9.sub <- c("2a) WCVISS ln(q) prior SD = 0.448",
#                             "2c) WCVISS ln(q) prior mean = ln(0.5)",
#                             "2d) WCVISS ln(q) prior mean = ln(1.0)",
#                             "2e) WCVISS ln(q) prior SD = 0.6",
#                             "2f) WCVISS ln(q) prior SD = 1.0")
#
#
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 9 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.9 <- c(file.path(model.dir,
#                                       "1_2a_3CD_rsoleq_0228sd0448"),
#                             file.path(model.dir,
#                                       "1_2b_3CD_q_all_unif"),
#                             file.path(model.dir,
#                                       "1_2c_3CD_q_05"),
#                             file.path(model.dir,
#                                       "1_2d_3CD_q_1"),
#                             file.path(model.dir,
#                                       "1_2e_3CD_q_cv06"),
#                             file.path(model.dir,
#                                       "1_2f_3CD_q_cv1"))
# sens.models.name.9 <- c("2a) WCVISS ln(q) SD = 0.448",
#                         "2b) No priors on ln(q)",
#                         "2c) WCVISS ln(q) prior mean = ln(0.5)",
#                         "2d) WCVISS ln(q) prior mean = ln(1.0)",
#                         "2e) WCVISS ln(q) prior SD = 0.6",
#                         "2f) WCVISS ln(q) prior SD = 1.0")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 10 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.10 <- c(file.path(model.dir,
#                                        "1_3a_3CD_Mprior_mean04_sd01"),
#                              file.path(model.dir,
#                                        "1_3b_3CD_Mprior_mean04_sd025"),
#                              file.path(model.dir,
#                                        "1_3c_3CD_Mprior_mean05_sd-25"))
# sens.models.name.10 <- c("3a) M prior mean = 0.4, SD = 0.1",
#                          "3b) M prior mean = 0.4, SD = 0.25",
#                          "3c) M prior mean = 0.5, SD = 0.25")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 11 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.11 <- c(file.path(model.dir,
#                                       "1_4a_3CD_hprior_unif"),
#                             file.path(model.dir,
#                                       "1_4b_3CD_hprior_085"))
# sens.models.name.11 <- c("4a) Steepness uniform prior",
#                          "4b) Steepness beta prior mean=0.85, SD=0.75")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 12 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.12 <- c(file.path(model.dir,
#                                        "1_5a_3CD_kage3"),
#                             file.path(model.dir,
#                                       "1_5b_3CD_old_growth_pars"))
# sens.models.name.12 <- c("5a) kage = 3y and update FW parameters",
#                          "5b) Use previous vonB pars")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 13 sub (3CD) Decision table figure
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.13.sub <- c(file.path(model.dir,
#                                            "1_2d_3CD_q_1"),
#                                  file.path(model.dir,
#                                            "1_2e_3CD_q_cv06"),
#                                  file.path(model.dir,
#                                            "1_3a_3CD_Mprior_mean04_sd01"),
#                                  file.path(model.dir,
#                                            "1_5a_3CD_kage3"),
#                                  file.path(model.dir,
#                                            "1_6b_3CD_sig015"),
#                                  file.path(model.dir,
#                                            "1_7b_3CD_sigW015")
#                                  )
#
# sens.models.name.13.sub <- c("2d) WCVISS ln(q) prior mean = ln(1.0)",
#                              "2e) WCVISS ln(q) prior SD = 0.6",
#                              "3a) M prior mean = 0.4, SD = 0.1",
#                              "5a) kage = 3y and update FW parameters",
#                              "6b) Fix sigma O = 0.15",
#                              "7b) Fix sigma W = 0.15")
#
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 13 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.13 <- c(file.path(model.dir,
#                                        "1_6a_3CD_sig01"),
#                              file.path(model.dir,
#                                        "1_6b_3CD_sig015"),
#                              file.path(model.dir,
#                                        "1_6c_3CD_sig025"),
#                              file.path(model.dir,
#                                        "1_6d_3CD_tau1"))
#
# sens.models.name.13 <- c("6a) Fix sigma O = 0.1",
#                          "6b) Fix sigma O = 0.15",
#                          "6c) Fix sigma O = 0.25",
#                          "6d) Fix sigma R = 1.0")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 14 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.14 <- c(file.path(model.dir,
#                                       "1_7a_3CD_sigW04"),
#                              file.path(model.dir,
#                                        "1_7b_3CD_sigW015"),
#                              file.path(model.dir,
#                                        "1_7c_3CD_noHistMeanWeight"))
# sens.models.name.14 <- c("7a) Fix sigma W = 0.4",
#                          "7b) Fix sigma W = 0.15",
#                          "7c) Remove historical mean weight data")
#
# ## -----------------------------------------------------------------------------
# ## Sensitivity models group 15 (3CD)
# ## -----------------------------------------------------------------------------
# sens.models.dir.name.15 <- c(file.path(model.dir,
#                                       "1_8a_3CD_InflateHistCatch_025"),
#                             file.path(model.dir,
#                                       "1_8b_3CD_InflateHistCatch_05"))
# sens.models.name.15 <- c("8a) Inflate pre-1996 catch by 25%",
#                         "8b) Inflate pre-1996 catch by 50%")
#
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
# retro.dir.names.5abcd <- c(file.path(base.model.5abcd.dir.name,
#                                    "Retrospective01"),
#                            file.path(base.model.5abcd.dir.name,
#                                      "Retrospective02"),
#                            file.path(base.model.5abcd.dir.name,
#                                      "Retrospective03"),
#                            file.path(base.model.5abcd.dir.name,
#                                     "Retrospective04"))
retro.dir.names <- c(retro.dir.names.3cd)#,
                    # retro.dir.names.5abcd)
retro.names <- c("- 1 year",
                 "- 2 years",
                 "- 3 years",
                 "- 4 years")

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  # base.model.5abcd <<- load.models(base.model.5abcd.dir.name)
  # desc.models.5abcd <<- load.models(desc.models.5abcd.dir.name)
  # avg.model.5abcd <<- avg.models(desc.models.5abcd)
  # # sens.models.0 <<- load.models(sens.models.dir.name.0)
  # # sens.models.1.sub <<- load.models(sens.models.dir.name.1.sub)
  # # sens.models.1.sub2 <<- load.models(sens.models.dir.name.1.sub2)
  # # sens.models.1 <<- load.models(sens.models.dir.name.1)
  # # sens.models.2 <<- load.models(sens.models.dir.name.2)
  # # sens.models.2.sub <<- load.models(sens.models.dir.name.2.sub)
  # # sens.models.3 <<- load.models(sens.models.dir.name.3)
  # # sens.models.4 <<- load.models(sens.models.dir.name.4)
  # # sens.models.5 <<- load.models(sens.models.dir.name.5)
  # # sens.models.6 <<- load.models(sens.models.dir.name.6)
  # sens.models.6.sub <<- load.models(sens.models.dir.name.6.sub)
  # # sens.models.7 <<- load.models(sens.models.dir.name.7)
  # sens.models.108 <<- load.models(sens.models.dir.name.108)

  base.model.3cd <<- load.models(base.model.3cd.dir.name)
  sens.models.11 <<- load.models(sens.models.dir.name.11)
  #avg.model.3cd <<- avg.models(desc.models.3cd)
  # sens.models.00 <<- load.models(sens.models.dir.name.00)
  # sens.models.8.sub <<- load.models(sens.models.dir.name.8.sub)
  # sens.models.8.sub2 <<- load.models(sens.models.dir.name.8.sub2)
  # sens.models.8 <<- load.models(sens.models.dir.name.8)
  # sens.models.9 <<- load.models(sens.models.dir.name.9)
  # sens.models.9.sub <<- load.models(sens.models.dir.name.9.sub)
  # sens.models.10 <<- load.models(sens.models.dir.name.10)
  # sens.models.11 <<- load.models(sens.models.dir.name.11)
  # sens.models.12 <<- load.models(sens.models.dir.name.12)
  # sens.models.13 <<- load.models(sens.models.dir.name.13)
  # sens.models.13.sub <<- load.models(sens.models.dir.name.13.sub) # model av
  # sens.models.14 <<- load.models(sens.models.dir.name.14)
  # sens.models.15 <<- load.models(sens.models.dir.name.15)


  # base.retro.models.5abcd <<- load.models(retro.dir.names.5abcd)
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
  # create.rdata.file(base.model.5abcd.dir.name,
  #                   ovwrt.rdata = ovwrt.base,
  #                   load.proj = TRUE,
  #                   burnin = burnin,
  #                   thin = thin,
  #                   low = confidence.vals[1],
  #                   high = confidence.vals[2],
  #                   verbose = ss.verbose)
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
  sens.models.names.list <- c(#unlist(desc.models.5abcd.dir.name),
                              unlist(desc.models.3cd.dir.name)#,
                              # unlist(sens.models.dir.name.0),
                              # unlist(sens.models.dir.name.1),
                              # unlist(sens.models.dir.name.2),
                              # unlist(sens.models.dir.name.3),
                              # unlist(sens.models.dir.name.4),
                              # unlist(sens.models.dir.name.5),
                              # unlist(sens.models.dir.name.6),
                              # unlist(sens.models.dir.name.7),
                              # unlist(sens.models.dir.name.108),
                              # unlist(sens.models.dir.name.00),
                              # unlist(sens.models.dir.name.8),
                              # unlist(sens.models.dir.name.9),
                              # unlist(sens.models.dir.name.10),
                              # unlist(sens.models.dir.name.11),
                              # unlist(sens.models.dir.name.12),
                              # unlist(sens.models.dir.name.13),
                              # unlist(sens.models.dir.name.14),
                              # unlist(sens.models.dir.name.15)
                              )
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

