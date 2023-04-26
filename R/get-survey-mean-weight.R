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
library(RColorBrewer)
library(ggsci)
library(here)

dir <- here("report/figures")
dir3cd <- file.path(dir,"3cd")
if(!dir.exists(dir)) dir.create(dir)
if(!dir.exists(dir3cd)) dir.create(dir3cd)
if(!dir.exists(dir5abcd)) dir.create(dir5abcd)

AREAS <- c("3CD") #"5ABCD",

TYPE <- "weighted"
# TYPE <- "raw"

for(AREA in AREAS){
  print(AREA)
  # L-W parameters
  #3CD
  if (AREA == "3CD") {
    .ALPHA <- 7.65616e-06
    .BETA <- 3.08
    SURVEY <- c("SYN WCVI")
    figdir <- dir3cd
  } else {
    #5ABCD
    .ALPHA <- 6.722839e-06
    .BETA <- 3.11
    SURVEY <- c("SYN QCS", "SYN HS")
    figdir <- dir5abcd
  }

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
    select(year,fishing_event_id, sample_id,grouping_code,length,weight) %>%
    mutate(weight_calc=.ALPHA*length^.BETA, weight=weight/1000) %>%
    left_join(catch_weight_summary)

# Now weight by catch
  # 1. get the mean weight in the samples, with the catch weight from the fishing event
  # Equivalent to Eq C.6 in the 2018 assessment
  Mean_wt_samples <- lengthwt_raw %>%
    group_by(year, sample_id, grouping_code) %>%
    summarize(mean_weight_calc=mean(weight_calc),
              sum_weight_calc=sum(weight_calc),
              catch_weight=catch_weight[1])

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

  Mean_weight_stratum <- Mean_wt_samples %>%
    group_by(year, grouping_code) %>%
    summarize(stratum_mean_wt=sum(mean_weight_calc*sum_weight_calc)/sum(sum_weight_calc),
              stratum_catch_wt=sum(catch_weight))

  #Equivalent to Eq C.8 in the 2018 assessment
  Annual_mean_wt_weighted <- Mean_weight_stratum %>%
    group_by(year) %>%
    summarize(annual_mean_weight=sum(stratum_mean_wt*stratum_catch_wt)/sum(stratum_catch_wt))

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
  ggsave(file.path(figdir,paste0("Measured_v_Calc_weights_",
                                 AREA,".png")))

  # Plot annual mean weights
  g <- Annual_mean_wt_raw %>%
    rename(raw=annual_mean_weight) %>%
    left_join(Annual_mean_wt_weighted) %>%
    rename(weighted=annual_mean_weight) %>%
    melt(id.vars="year", variable.name="measurement_type", value.name="mean_weight") %>%
    ggplot()+
    geom_line(aes(x=year, y=mean_weight, colour=measurement_type,
                  linetype=measurement_type), size=1.5)+
    ylim(0,2.5)+
    gfplot::theme_pbs()+
    #scale_colour_brewer(palette = "Dark2")+
    #scale_color_aaas()+
    scale_colour_viridis_d()+
    theme(title = element_text(size=12, face="bold"))+
    theme(axis.text.x = element_text(size=12))+
    theme(axis.text.y = element_text(size=12))+
    theme(axis.title.x = element_text(size=14))+
    theme(axis.title.y = element_text(size=14))+
    theme(legend.text = element_text(size=12))+
    theme(legend.title = element_text(size=13))+
    labs(title = paste(AREA), y = "Survey mean weight", x = "Year")
  ggsave(file.path(figdir,paste0("Weighted_v_Raw_weights_",
                                 AREA,".png")))

########################################
  # Now fit a linear model to predict commercial mw from survey mw
  # Follow Sean's advice
  cmw <- readr::read_csv(here::here("data/generated/all-commercial-mean-weight.csv"))
  cmw <- dplyr::filter(cmw, area == AREA) %>%
    rename(commercial_mw = mean_weight) %>%
    select(-area)

  if (AREA == "5ABCD") {
    cmw$year <- cmw$year
  }

  if (TYPE == "weighted") {
    dat1 <- Annual_mean_wt_weighted %>%
      rename(survey_mw = annual_mean_weight) %>%
      full_join(cmw) %>%
      arrange(year) %>%
      filter(year >= 2000)
    # # View(dat1)
  } else {
    dat1 <- Annual_mean_wt_raw %>%
      rename(survey_mw = annual_mean_weight) %>%
      full_join(cmw) %>%
      arrange(year) %>%
      filter(year >= 2000)
    # View(dat1)
  }

  g <- tidyr::pivot_longer(dat1, cols = 2:3) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(year, value, colour = name)) +
    geom_vline(xintercept = 2000:2021, lty = 1, col = "grey80") +
    geom_point(size=1.4) +
    geom_line(size=1.4) +
    ggtitle(paste(AREA, TYPE))+
    theme_light()+
    ylim(0,3.2)+
    #scale_colour_brewer(palette = "Dark2")+
    #scale_color_aaas()+
    scale_colour_viridis_d()+
    theme(title = element_text(size=12, face="bold"))+
    theme(axis.text.x = element_text(size=12))+
    theme(axis.text.y = element_text(size=12))+
    theme(axis.title.x = element_text(size=14))+
    theme(axis.title.y = element_text(size=14))+
    theme(legend.text = element_text(size=12))+
    theme(legend.title = element_text(size=13))+
    labs(title = paste(AREA), y = "Mean weight", x = "Year")
  ggsave(file.path(figdir,paste0("Comm_v_Survey_weights_",
                                 AREA,".png")))

  r <- range(log(c(dat1$survey_mw, dat1$commercial_mw)), na.rm = TRUE)

  g <- ggplot(dat1, aes(log(survey_mw), log(commercial_mw))) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE)+
    ggrepel::geom_text_repel(aes(label = year), size = 4) +
    geom_abline(intercept = 0, slope = 1) +
    #coord_fixed(xlim = c(r[1], r[2]), ylim = c(r[1], r[2])) +
    ggtitle(paste(AREA, TYPE))+
    gfplot::theme_pbs()+
    theme(title = element_text(size=12, face="bold"))+
    theme(axis.text.x = element_text(size=12))+
    theme(axis.text.y = element_text(size=12))+
    theme(axis.title.x = element_text(size=14))+
    theme(axis.title.y = element_text(size=14))+
    theme(legend.text = element_text(size=12))+
    theme(legend.title = element_text(size=13))+
    ylim(0,1.2)+xlim(0,1.2)+
    labs(title = paste(AREA), x = "Ln survey mean weight", y = "Ln comm mean weight")
  ggsave(file.path(figdir,paste0("lnSurvey_v_lnCom_with_lm_fit_",
                                 AREA,".png")))

  ####################################################
  # predict commercial mw from regression
  if(AREA=="3CD"){
    nosurvyr <- c(2017,2019,2020)
  }else{
    nosurvyr <- c(2018,2020)
  }

  GLM <- glm(commercial_mw ~ log(survey_mw),
             family = Gamma(link = "log"),
             data = dat1)
  summary(GLM)

  newdata <- dat1 %>%
    dplyr::filter(!is.na(survey_mw)) %>%
    as.data.frame()

  pred_commercial_mw <- predict(GLM, newdata, type="response")

  comparedata <- newdata %>%
    cbind(pred_commercial_mw)

  comparedata_allyrs  <-
    rbind(cbind(nosurvyr,
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)))) %>%
    `colnames<-`(colnames(comparedata)) %>%
    rbind(comparedata) %>%
    arrange(year)

  # put back the observed commercial mean weights for years with no survey
  if(AREA=="3CD"){
    comparedata_allyrs[which(comparedata_allyrs$year==2019),3] <- cmw[which(cmw$year==2019),2]
  }else{
    comparedata_allyrs[which(comparedata_allyrs$year==2018),3] <- cmw[which(cmw$year==2018),2]
  }

  g1 <- comparedata_allyrs %>%
    melt(id.vars="year", variable.name="Obs_vs_Pred", value.name="commercial_mw") %>%
    ggplot()+
    geom_point(aes(x=year, y=commercial_mw, colour=Obs_vs_Pred), size=2.5)+
    geom_line(aes(x=year, y=commercial_mw, colour=Obs_vs_Pred), lwd=1, lty=2)+
    theme_light()+
    #scale_colour_brewer(palette = "Dark2")+
    scale_colour_viridis_d()+
    #scale_color_aaas()+
    theme(title = element_text(size=12, face="bold"))+
    theme(axis.text.x = element_text(size=10))+
    theme(axis.text.y = element_text(size=12))+
    theme(axis.title.x = element_text(size=14))+
    theme(axis.title.y = element_text(size=14))+
    theme(legend.text = element_text(size=12))+
    theme(legend.title = element_text(size=13))+
    theme(legend.position = "right")+
    ylim(0,3.5)+
    scale_x_continuous(breaks=seq(min(comparedata_allyrs$year),max(comparedata_allyrs$year), by=2))+
    labs(title = paste(AREA), y = "Mean weight", x = "Year")
  #g1
  ggsave(file.path(figdir,paste0("Compare_obs_v_predicted_",
                                 AREA,".png")))

  # Now need to interpolate for years with no survey data
  # Only need to do this for years without comm samples
  comparedata_interpolate <- comparedata_allyrs %>%
    select(year,pred_commercial_mw)

  # NOW interpolate values between 2018 and 2020 for updating the model files
  # Okay to assume we had a 2021 survey
  if(AREA=="3CD"){
    # Interpolate between 2018 and 2021
    interpolate <- comparedata_interpolate %>%
      filter(year%in%2018:2021) %>%
      approx(xout=2019:2020)
    #Add interpolated value to dataframe
    comparedata_interpolate[which(comparedata_interpolate$year %in% 2019:2020),2]<-interpolate$y
  }else{
    #1. Interpolate between 2017 and 2019
    interpolate <- comparedata_interpolate %>%
      filter(year%in%2017:2019) %>%
      approx(xout=2018)
    #Add interpolated value to dataframe
    comparedata_interpolate[which(comparedata_interpolate$year==2018),2]<-interpolate$y

    #2. Interpolate between 2019 and 2021
    interpolate <- comparedata_interpolate %>%
      filter(year%in%2019:2021) %>%
      approx(xout=2020)
    #Add interpolated value to dataframe
    comparedata_interpolate[which(comparedata_interpolate$year==2020),2]<-interpolate$y
  }

  # write out the values
  readr::write_csv(comparedata_allyrs,
            file.path(figdir,paste0("Comm_v_Survey_weights_",
            AREA,"_all_compare.csv")))

  readr::write_csv(comparedata_interpolate,
                   file.path(figdir,paste0("Pred_comm_weight_with_interpolation_",
                                           AREA,".csv")))

} # End AREA loop
