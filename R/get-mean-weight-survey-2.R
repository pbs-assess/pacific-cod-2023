# Code to get the annual mean weight in the survey. Try to mimic the commercial mean
#  weight code as much as possible

# Copy of get-mean-weight-survey.R to address questions from Paul Starr: July 20, 2023

# This script is called by get-iscam-inputs.R

# This was for the 2022 technical working group meeting to look at whether we can
# calibrate the survey mean weight to the commercial mean weight, to possibly
# use survey mean weight going forward despite different selectivities. The TWG
# thought the calibration worked for 3CD but not 5ABCD

# June 2 2022. Robyn Forrest (RF)
# Updated April 28 2023. RF
# Updated July 20 2023. RF

# TODO: Check that the catch weighting makes sense
# Well, none of it really makes sense because length sampling has been
# length-stratified for most years of the survey!!

AREA <- "3CD"
print(AREA)
Years <- 2004:2022

mytheme <- gfplot::theme_pbs() +
  theme(title = element_text(size=12, face="bold"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=13))
theme_set(mytheme)

TYPE <- "weighted"
# TYPE <- "raw"

# L-W parameters
#3CD
.ALPHA <- 7.65616e-06
.BETA <- 3.08
SURVEY <- c("SYN WCVI")

catch_weight_summary <- dat$survey_sets %>%
  select(c(year, fishing_event_id, sample_id, grouping_code,density_kgpm2, catch_weight)) %>%
  filter(!is.na(sample_id))

# get survey lengths (unweighted)
# weight_calc=.ALPHA3*length^.BETA3 is Eq C.5 in the 2018 assessment
lengthwt_raw <- dat$survey_samples %>%
  filter(survey_abbrev %in% SURVEY,
         usability_code %in% c(0, 1, 2, 6),
         !is.na(length)) %>%
  select(year,fishing_event_id, sample_id,specimen_id, sex,grouping_code,length,weight) %>%
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
survey_mw_weighted <- Mean_weight_stratum %>%
  group_by(year) %>%
  summarize(survey_mw_weighted=sum(stratum_mean_wt*stratum_catch_wt)/sum(stratum_catch_wt))

# Now do an unweighted version
# Get the annual mean weight (unweighted by catch weight)
survey_mw_raw <- lengthwt_raw %>%
  group_by(year) %>%
  summarize(survey_mw_raw=mean(weight_calc))

# For the record, write out all the mean weight index observations
# get commercial mean weight that was generated in get-mean-weight.R
cmw <- read_csv(here::here("data/generated/commercial_mean_weight_3CD.csv"))
allmw <- cmw %>%
 rename(commercial_mw = mean_weight) %>%
  filter(year>=Years[1]) %>%
  full_join(survey_mw_weighted) %>%
  full_join(survey_mw_raw) %>%
  arrange(year)

# Add rows for missing years
missing_years <- Years[!Years %in% allmw$year]
missing_rows <- data.frame(year = missing_years,
                          commercial_mw=NA,
                          n_samples = NA,
                          n_specimens = NA,
                          survey_mw_raw=NA,
                          survey_mw_weighted=NA)

allmw <- allmw %>%
  rbind(missing_rows) %>%
  arrange(year)

write_csv(allmw,file.path(generatedd,"all_mean_weight_observations.csv"))

# plot measured weight against calculated weight
# raw
g <- lengthwt_raw %>%
  filter(!is.na(weight)) %>%
  ggplot() +
  geom_point(aes(x=weight, y=weight_calc), colour="darkblue") +
  labs(title = paste(AREA), y = "Calculated weight from length", x = "Measured weight")
ggsave(file.path(generatedd,paste0("Measured_v_Calc_Weights_",
                               AREA,".png")))
# Plot annual mean weights
g <- survey_mw_raw %>%
  rename(raw=survey_mw_raw) %>%
  left_join(survey_mw_weighted) %>%
  rename(weighted=survey_mw_weighted) %>%
  melt(id.vars="year", variable.name="measurement_type", value.name="mean_weight") %>%
  ggplot()+
  geom_line(aes(x=year, y=mean_weight, colour=measurement_type,
                linetype=measurement_type), size=1.5)+
  ylim(0,2.5)+
  #scale_colour_brewer(palette = "Dark2")+
  #scale_color_aaas()+
  scale_colour_viridis_d()+
  labs(title = paste(AREA), y = "Survey mean weight", x = "Year")
ggsave(file.path(generatedd,paste0("Weighted_v_Raw_Weights_Survey",
                               AREA,".png")))

# ########################################
# Now fit a linear model to predict commercial mw from survey mw
# Follow Sean's advice

# remove 2017
# From 2022 TWG report 1:
# "In Area 3CD, only 4 samples were taken in 2017 (total 300 fish), no samples were
# taken in 2018, and only two samples were taken in 2019 (total 360 fish).
# The 2017 mean weight value was anomalously high (3.024 kg) and was not used in the
#2020 stock assessment update (DFO 2021)."

# Also remove 2019. It is based on 2 samples (n=360).
# Note 2019 was used in the 2020 assessment, but probably shouldn't have been.

# 1. pull the commercial mean weight index generated in get-mean-weight.R
cmw <- readr::read_csv(here::here("data/generated/commercial_mean_weight_3CD.csv"))
# get the 2019 value in case we need it again
cmw2019 <- cmw %>%
  filter(year %in% c(2017, 2019))

cmw <- cmw %>%
  rename(comm_mean_weight=mean_weight) %>%
  filter(!year %in% c(2017, 2019))

# 2. Join the commercial and survey mean weight indices into one df
# filter for years > 2000:
#   sample sizes really increased in 2001, see 2022 TWG_report.pdf appendix A
if (TYPE == "weighted") {
    dat1 <- survey_mw_weighted %>%
      rename(survey_mean_weight = survey_mw_weighted) %>%
      full_join(cmw) %>%
      arrange(year) %>%
      filter(year >= 2000)
    # # View(dat1)
 } else {
    dat1 <- survey_mw_raw %>%
      rename(survey_mean_weight = survey_mw_raw) %>%
      full_join(cmw) %>%
      arrange(year) %>%
      filter(year >= 2000)
    # View(dat1)
 }

# 3. Plot time series of the two indices for years since 2000
Title <- paste(AREA, TYPE, ": 2017 and 2019 comm. removed due to low sample size")

# pal <- RColorBrewer::brewer.pal(3, "Dark2")
pal <- unname(colorBlindness::availableColors()[-1])
 g <- tidyr::pivot_longer(dat1, cols = 2:3) %>%
   mutate(name = gsub("survey_mean_weight", "Survey", name)) |>
   mutate(name = gsub("comm_mean_weight", "Commercial", name)) |>
    filter(!is.na(value)) %>%
    ggplot(aes(year, value, colour = name)) +
    geom_vline(xintercept = 2000:2022, lty = 1, col = "grey90") +
   # theme(panel.grid.major.x = element_line(colour = "grey90")) +
   # theme(panel.grid.minor.x = element_line(colour = "grey90")) +
    # geom_point(size=3.5) +
    geom_line(size=1.4) +
    ylim(0,3)+
    # scale_color_aaas()+
   scale_colour_manual(values = c("Commercial" = pal[1], "Survey" = pal[2])) +
    labs(title = Title, y = "Mean weight", x = "Year") +
   geom_point(data = dat1, mapping = aes(year, comm_mean_weight, size = n_samples), inherit.aes = FALSE, pch = 21, na.rm = TRUE) +
   scale_size_area(name = "Sampling events") +
   labs(colour = "Type")
 g
 ggsave(file.path(generatedd,paste0("Comm_v_Survey_weights_",
                                 AREA,".png")), width = 7, height = 4)

 #Make the plot again with points for the survey
 pal <- unname(colorBlindness::availableColors()[-1])
 cols <- c("Commercial" = pal[1], "Survey" = pal[2])
 g <- dat1 %>%
   ggplot(aes(year)) +
   geom_vline(xintercept = 2000:2022, lty = 1, col = "grey90") +
   # theme(panel.grid.major.x = element_line(colour = "grey90")) +
   # theme(panel.grid.minor.x = element_line(colour = "grey90")) +
   # geom_point(size=3.5) +
   geom_line(aes(x=year, y=comm_mean_weight, colour="Commercial"),size=1.4) +
   geom_point(data = dat1, mapping = aes(year, comm_mean_weight, size = n_samples), inherit.aes = FALSE, pch = 21, na.rm = TRUE) +
   geom_point(data = dat1, mapping = aes(year, survey_mean_weight, colour="Survey"), size = 4, inherit.aes = FALSE, pch = 19, na.rm = TRUE) +
   ylim(0,3)+
   labs(title = Title, y = "Mean weight (kg)", x = "Year") +
   scale_size_area(name = "Comm. sampling events") +
   scale_color_manual(name="Type",values = c("Commercial" = pal[1], "Survey" = pal[2]),
                      breaks=c('Commercial', 'Survey')) +
   scale_x_continuous(breaks=(seq(2000,2022,by=2)))+
   labs(colour = "Type")
 g
 ggsave(file.path(generatedd,paste0("Comm_v_Survey_weights_",
                                    AREA,".png")), width = 7, height = 4)

# 4. Plot the two indices against each other (log space)
#    Note that the last pair of survey and commercial index values was in 2016
#    No survey in 2017, 2019 or 2020. No commercial samples in 2018.

r <- range(log(c(dat1$survey_mean_weight, dat1$comm_mean_weight)), na.rm = TRUE)

#
# ####################################################
# predict commercial mw from a glm
 nosurvyr <- c(2017,2019,2020)
 # 1. Fit glm
 GLM <- glm(comm_mean_weight ~ log(survey_mean_weight),
   family = Gamma(link = "log"),
   data = dat1)

 # do same with sdmTMB to get easy quantile residuals and shape parameter:
 dat_noNA <- subset(dat1, !is.na(survey_mean_weight) & !is.na(comm_mean_weight))
 m1 <- sdmTMB::sdmTMB(
   comm_mean_weight ~ log(survey_mean_weight),
   family = Gamma(link = "log"), spatial = FALSE,
   data = dat_noNA
 )

 r <- residuals(m1)
 rr <- qqnorm(r, plot.it = FALSE)
 g <- ggplot(data.frame(x = rr$x, y = rr$y), aes(x, y)) +
   geom_point() +
   labs(x = "Theoretical quantiles", y = "Sample quantiles") +
   geom_abline(intercept = 0, slope = 1) +
   # theme_light() +
   coord_fixed()
 ggsave(file.path(generatedd,paste0("qq-resids",
   AREA,".png")), width = 3.8, height = 3.9)

 p <- sdmTMB::get_pars(m1)
 shape <- exp(p$ln_phi)
 # 1/CV^2 = shape
 # CV^2 = 1 / shape
 # CV = 1 / sqrt(shape)
 cv <- 1 / sqrt(shape)

 nd <- data.frame(survey_mean_weight = seq(min(dat_noNA$survey_mean_weight), max(dat_noNA$survey_mean_weight), length.out = 400))
 suppressMessages({
   p <- predict(m1, se_fit = TRUE, newdata = nd)
 })
 p$lwr2 <- exp(p$est - qnorm(0.75) * p$est_se)
 p$upr2 <- exp(p$est + qnorm(0.75) * p$est_se)
 p$lwr <- exp(p$est - qnorm(0.975) * p$est_se)
 p$upr <- exp(p$est + qnorm(0.975) * p$est_se)
 p$est <- exp(p$est)

 g <- ggplot(dat1, aes(survey_mean_weight, comm_mean_weight, colour = year)) +
   #coord_fixed(xlim = c(r[1], r[2]), ylim = c(r[1], r[2])) +
   # ylim(0,1.2)+xlim(0,1.2)+
   geom_ribbon(data = p, aes(x = survey_mean_weight, ymin = lwr, ymax = upr), inherit.aes = FALSE, fill = "grey90") +
   geom_ribbon(data = p, aes(x = survey_mean_weight, ymin = lwr2, ymax = upr2), inherit.aes = FALSE, fill = "grey80") +
   geom_line(data = p, aes(x = survey_mean_weight, y = est), inherit.aes = FALSE) +
   geom_point(aes(size = n_samples), pch = 19, alpha = 0.3) +
   geom_point(aes(size = n_samples), pch = 21) +
   scale_size_area(name = "Sampling events", max_size = 10) +
   # stat_smooth(method = "lm", se = FALSE)+
   ggrepel::geom_text_repel(aes(label = year), size = 4, point.padding = 12) +
   scale_colour_viridis_c() +
   geom_abline(intercept = 0, slope = 1, colour = "grey40", lty = 2) +
   # labs(title = paste(AREA, TYPE), x = "Survey mean weight", y = "Commercial mean weight", colour = "Year") +
   labs(x = "Survey mean weight", y = "Commercial mean weight", colour = "Year") +
   scale_x_log10() +
   scale_y_log10() +
   coord_fixed()
   # theme_light()

 ggsave(file.path(generatedd,paste0("lnSurvey_v_lnCom_with_lm_fit_",
   AREA,".png")), width = 5.5, height = 4.5)

 if (FALSE) {
   summary(GLM)

   DHARMa::testDispersion(GLM)
   sim <- DHARMa::simulateResiduals(fittedModel = GLM, plot = FALSE)
   plot(sim)
  }

# 2. set up a new df for the predictions and predict comm mean weight from
# survey mean weight for 2018, 2021 and 2022
newdata <- dat1 %>%
    dplyr::filter(!is.na(survey_mean_weight)) %>%
    as.data.frame()
#
pred_comm_mean_weight <- predict(GLM, newdata, type="response")
#
comparedata <- newdata %>%
    cbind(pred_comm_mean_weight)

# put back the years with no survey data
comparedata_allyrs  <-
    rbind(cbind(nosurvyr,
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)),
                rep(NA,length(nosurvyr)))) %>%
    `colnames<-`(colnames(comparedata)) %>%
    rbind(comparedata) %>%
    arrange(year)

#   # put back the observed commercial mean weights for years with no survey
#   comparedata_allyrs[which(comparedata_allyrs$year==2019),3] <- cmw[which(cmw$year==2019),2]

# pal <- ggsci::pal_aaas()(3)
  g1 <- comparedata_allyrs %>%
    as.data.frame() %>%
    select(-n_samples, -n_specimens) |>
    melt(id.vars="year", variable.name="obs_vs_pred", value.name="comm_mean_weight") %>%
    mutate(across("obs_vs_pred", str_replace, "survey_mean_weight", "Survey"),
      across("obs_vs_pred", str_replace, "pred_comm_mean_weight", "Predicted commercial"),
           across("obs_vs_pred", str_replace, "comm_mean_weight", "Commercial")) %>%
    ggplot()+
    geom_point(aes(x=year, y=comm_mean_weight, colour=obs_vs_pred), size=2.5)+
    geom_line(aes(x=year, y=comm_mean_weight, colour=obs_vs_pred), lwd=1, lty=1)+
    # scale_color_aaas()+
    scale_colour_manual(values = c("Commercial" = pal[1], "Survey" = pal[2], "Predicted commercial" = pal[3])) +
    theme(legend.position = "right")+
    ylim(0,3)+
    scale_x_continuous(breaks=seq(min(comparedata_allyrs$year),max(comparedata_allyrs$year), by=2))+
    labs(title = paste(AREA, TYPE), y = "Mean weight", x = "Year", colour = "Time series type")
  g1
  ggsave(file.path(generatedd,paste0("Compare_Obs_v_Predicted_Weight",
                                 AREA,".png")), width = 7.5, height = 4)

# bayesian posterior predictions?
  if (FALSE) {

    library(brms)
    fit <- brm(
      comm_mean_weight ~ log(survey_mean_weight),
      family = Gamma(link = "log"),
      data = dat1,
      prior = c(
        prior(normal(0, 1), class = "b"),
        prior(normal(0, 20), class = "Intercept")
      ),
    )
    fit
    plot(fit)

    # with/without obs. error?
    # pred_mean_weight <- posterior_predict(fit, newdata = newdata)
    pred_mean_weight <- exp(posterior_linpred(fit, newdata = newdata))

    # fine most positive extreme slopes in last 3 time steps:
    w <- reshape2::melt(pred_mean_weight) |>
      rename(iter = Var1, year = Var2)
    slopes <- w |>
      filter(year >= 8) |> # predicted years
      group_by(iter) |>
      group_split() |>
      purrr::map_dfr(function(.x) {
        m <- lm(value ~ year, data = .x)
        data.frame(slope = coef(m)[[2]], iter = .x$iter[1])
      }) |>
      arrange(slope)

    hist(slopes$slope, breaks = 100)
    abline(v = 0, lty = 2, col = "red")

    qs <- quantile(slopes$slope, probs = c(0.05, 0.5, 0.95))
    # slopes[slopes$slope > qs[[3]], ]
    pos <- slopes[slopes$slope > qs[[3]], ]

    x <- comparedata_allyrs
    plot(x$year, x$comm_mean_weight, col = "red", ylim = c(0.8, 3.5), xlab = "Year", ylab = "Mean weight")
    lines(x$year, x$comm_mean_weight, col = "red")
    points(newdata$year, newdata$survey_mean_weight, col = "blue")
    lines(newdata$year, newdata$survey_mean_weight, col = "blue")
    points(x$year, x$pred_comm_mean_weight, col = "darkgreen")
    lines(x$year, x$pred_comm_mean_weight, col = "darkgreen")
    for (i in 1:500) {
      jit <- jitter(newdata$year, amount = 0.2)
      points(jit, pred_mean_weight[i,], col = "#00640010")
      lines(jit, pred_mean_weight[i,], col = "#00640005")
    }
    legend("topleft", legend = c("Commercial", "Survey", "Predicted commercial"), col = c("red", "blue", "darkgreen"), pch = c(21, 21, 21))

    #####
    for (i in seq_along(unique(pos$iter))) {
      xx <- dplyr::filter(w, iter == unique(pos$iter)[i])
      lines(newdata$year, xx$value, col = "#BF40BF20", lwd = 1)
    }
    #####

    p <- as.data.frame(fit)
    .x <- seq(log(min(newdata$survey_mean_weight)), log(max(newdata$survey_mean_weight)), length.out = 100)
    plot(log(dat1$survey_mean_weight), dat1$comm_mean_weight)
    for (i in 1:1000) {
      lines(.x, exp(p$b_Intercept[i] + p$b_logsurvey_mean_weight[i] * .x), col = "#00000010")
    }

    ci <- t(apply(pred_mean_weight, 2, quantile, probs = c(0.25, 0.75))) |>
      as.data.frame() |>
      mutate(obs = semi_join(comparedata_allyrs, newdata) |> pull(comm_mean_weight)) |>
      mutate(covered = obs > `25%` & obs < `75%`)
    ci
    mean(ci$covered, na.rm = TRUE)

    brms::pp_check(fit, ndraws = 200)
    brms::pp_check(fit, ndraws = 200, type = "ecdf_overlay")
  }

# Now need to interpolate for years with no survey data
  # Only need to do this for years without comm samples
  # MAY OR MAY NOT CHOOSE TO USE THESE VALUES: RUN MODELS WITH AND WITHOUT INTERPOLATION
  pred_mean_weight_no_interpolate <- comparedata_allyrs %>%
    select(year,pred_comm_mean_weight)
  pred_mean_weight_interpolate <- comparedata_allyrs %>%
    select(year,pred_comm_mean_weight)

# Interpolate values between 2018 and 2020 for updating the model files
    # Interpolate between 2018 and 2021
    interpolate <- pred_mean_weight_interpolate %>%
      filter(year%in%2018:2021) %>%
      approx(xout=2019:2020)

    #Add interpolated value to dataframe
    pred_mean_weight_interpolate[which(pred_mean_weight_interpolate$year %in% 2019:2020),2]<-interpolate$y

# write out the values
  write_csv(comparedata_allyrs,
           file.path(generatedd,paste0("Comm_v_Survey_Weights_",
           AREA,"_all_compare.csv")))

  write_csv(pred_mean_weight_interpolate,
                    file.path(generatedd,paste0("Pred_comm_weight_with_interpolation_",
                                            AREA,".csv")))
  write_csv(pred_mean_weight_no_interpolate,
            file.path(generatedd,paste0("Pred_comm_weight_without_interpolation_",
                                        AREA,".csv")))

#============================================================================================
# Extra analyses for Paul Starr
#============================================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1a. 2017 index point
  # get the number of samples and number of vessels from 2017 to 2019
ncomm_samples <- dat$commercial_samples %>%
    select(year,major_stat_area_name,vessel_id,gear_desc,species_code,
           fishing_event_id, sample_id, specimen_id) %>%
    filter(year %in% c(2017,2019),
           major_stat_area_name %in% c("3C: S.W. VANCOUVER ISLAND","3D: N.W. VANCOUVER ISLAND")) %>%
    group_by(year) %>%
    summarize(nsamples=n_distinct(sample_id),
              nvessels=n_distinct(vessel_id))

ncomm_specimens <- dat$commercial_samples %>%
  select(year,major_stat_area_name,vessel_id,gear_desc,species_code,
         fishing_event_id, sample_id, specimen_id) %>%
  filter(year %in% c(2017,2019),
         major_stat_area_name %in% c("3C: S.W. VANCOUVER ISLAND","3D: N.W. VANCOUVER ISLAND")) %>%
  group_by(year,sample_id) %>%
  summarize(nlengths=n_distinct(specimen_id))


  # 1b Update L-W parameters, using the same rlm model as in 2018
# Add usability code = 1 for fit_length_weight, and convert weight to grams (plot converts to kg)

lwdat <- lengthwt_raw %>%
    mutate(usability_code=1,
           weight=weight*1000)

# males and females
LWMf <- gfplot::fit_length_weight(lwdat, sex="female", method = "rlm")
LWMm <- gfplot::fit_length_weight(lwdat, sex="male", method = "rlm")
plot_length_weight(object_female=LWMf, object_male=LWMm, pt_alpha=1)
ggsave(file.path(generatedd,paste0("New_LW_fit_",
                                   AREA,".png")), width = 7.5, height = 4)

# both sexes
LWM <- gfplot::fit_length_weight(lwdat, sex="all", method = "rlm")
plot_length_weight(object_all=LWM, pt_alpha=1)
ggsave(file.path(generatedd,paste0("New_LW_fit_both_sex_",
                                   AREA,".png")), width = 7.5, height = 4)


lwa <- signif(exp(LWM$model$coefficients[1]),6)
lwb <- round(LWM$model$coefficients[2],2)

# 2a Residual plot of calc weight vs obs weight
M1 <- lm(weight_calc~weight,
         data = lengthwt_raw)
R1 <- rstandard(M1)

# plot model fit
# base R
# par(mfrow = c(1,1))
# plot(lengthwt_raw$weight_calc, lengthwt_raw$weight)
# abline(M1, col=2, lwd=2)

# ggplot
g <- lengthwt_raw %>%
  filter(!is.na(weight)) %>%
  ggplot(aes(x=weight, y=weight_calc)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)+
  labs(title = paste(AREA), y = "Calculated weight from length", x = "Measured weight")
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_with_lmfit_",
                                   AREA,".png")), width = 7.5, height = 4)

# plot basic diagnostics
# par(mfrow = c(2,2))
# plot(M1)

# histogram of residuals
g2 <- R1 %>%
  as.data.frame() %>%
  ggplot(aes(x=R1)) +
  geom_histogram(binwidth=0.1,
                 colour="black", fill="lightgray")+
  xlab("Residuals: calc weight vs obs weight")
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_residuals_hist_",
                                   AREA,".png")), width = 7.5, height = 4)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In log space
M2 <- lm(log(weight_calc)~log(weight),
         data = lengthwt_raw)
R2 <- rstandard(M2)

# plot model fit
# base R
# par(mfrow = c(1,1))
# plot(log(lengthwt_raw$weight_calc), log(lengthwt_raw$weight))
# abline(M2, col=2, lwd=2)

# ggplot
g <- lengthwt_raw %>%
  filter(!is.na(weight)) %>%
  mutate(log_weight_calc=log(weight_calc),
         log_weight=log(weight)) %>%
  ggplot(aes(x=log_weight, y=log_weight_calc)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)+
  labs(title = paste(AREA), y = "Calculated log(weight from length)", x = "log(observed weight)")
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_with_lmfit_LOG_",
                                   AREA,".png")), width = 7.5, height = 4)


# plot basic diagnostics
# par(mfrow = c(1,1))
# plot(M2)

# histogram of residuals
g2 <- R2 %>%
  as.data.frame() %>%
  ggplot(aes(x=R2)) +
  geom_histogram(binwidth=0.1,
                 colour="black", fill="lightgray")+
  xlab("Residuals: log calc weight vs log obs weight")
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_residuals_hist_LOG_",
                                   AREA,".png")), width = 7.5, height = 4)
# Scatter plot of residuals
g3 <- R2 %>%
  as.data.frame() %>%
  rename(y='.') %>%
  mutate(x=1:length(y)) %>%
  ggplot()+
    geom_point(aes(x=x,y=y))+
    geom_hline(yintercept=0, colour=2)+
    xlab("Index") + ylab("Residuals: log calc weight vs log obs weight")+
    theme(axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"))
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_residuals_points_LOG_",
                                   AREA,".png")), width = 7.5, height = 4)

# Compare number of calculated to observed samples
compare <- lengthwt_raw %>%
  select(year, weight, weight_calc) %>%
  group_by(year) %>%
  summarise(across(everything(), ~ sum(!is.na(.))))

nwobs <- sum(compare$weight)
nwcalc <- sum(compare$weight_calc)
nwdiff <- nwcalc-nwobs

g <- compare %>%
  rename(weight_obs=weight) %>%
  melt(id="year", variable.name="Weight_measure", value.name="Nspecimens") %>%
  rename("Year"=year) %>%
  ggplot(aes(x=Year,y=Nspecimens,fill=Weight_measure)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018,2020,2022))
ggsave(file.path(generatedd,paste0("Measured_v_Calc_weights_counts_",
                                   AREA,".png")), width = 7.5, height = 4)

# There are 934 fewer observed weights than calculated weights

# Re-calculate the survey mean weight index using observed weights
# 1. get the mean weight in the samples, with the catch weight from the fishing event
# Equivalent to Eq C.6 in the 2018 assessment
Mean_wt_samples_obs <- lengthwt_raw %>%
  group_by(year, sample_id, grouping_code) %>%
  filter(!is.na(weight)) %>%
  summarize(mean_weight=mean(weight),
            sum_weight=sum(weight),
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

Mean_weight_stratum_obs <- Mean_wt_samples_obs %>%
  group_by(year, grouping_code) %>%
  summarize(stratum_mean_wt=sum(mean_weight*sum_weight)/sum(sum_weight),
            stratum_catch_wt=sum(catch_weight))

#Equivalent to Eq C.8 in the 2018 assessment
survey_mw_weighted_obs <- Mean_weight_stratum_obs %>%
  group_by(year) %>%
  summarize(survey_mw_weighted_obs=sum(stratum_mean_wt*stratum_catch_wt)/sum(stratum_catch_wt))

# Now bind these with the index from the calculated weights
survey_mw_compare <- survey_mw_weighted %>%
  full_join(survey_mw_weighted_obs) %>%
  rename("weight_calc"=survey_mw_weighted,
         "weight_obs"=survey_mw_weighted_obs) %>%
  melt(id="year", variable.name = "Method", value.name="Mean_weight_index_kg") %>%
  ggplot() +
  geom_point(aes(x=year, y=Mean_weight_index_kg, colour=Method), pch=19,size=3)+
  ylim(0,3)+
  scale_x_continuous(breaks=c(2004,2006,2008,2010,2012,2014,2016,2018,2020,2022))
ggsave(file.path(generatedd,paste0("Survey_annual_mean_weight_compare_",
                                   AREA,".png")), width = 7.5, height = 4)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2c Look at whether there are tows with multiple sample IDs
sample_ids <- lengthwt_raw %>%
  select(year, fishing_event_id,sample_id) %>%
  group_by(year, fishing_event_id) %>%
  summarize(unique_sample_id=n_distinct(sample_id)) %>%
  arrange(desc(unique_sample_id))

#cat("The maximum number of samples per tow is", max(sample_ids$unique_sample_id))

# NO, no tows with more than one sample

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2d

#============================================================================================


