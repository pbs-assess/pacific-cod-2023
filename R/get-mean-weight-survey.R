# Code to get the annual mean weight in the survey. Try to mimic the commercial mean
#  weight code as much as possible

# This script is called by get-iscam-inputs.R

# This was for the 2022 technical working group meeting to look at whether we can
# calibrate the survey mean weight to the commercial mean weight, to possibly
# use survey mean weight going forward despite different selectivities. The TWG
# thought the calibration worked for 3CD but not 5ABCD

# June 2 2022. Robyn Forrest (RF)
# Updated April 28 2023. RF

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
ggsave(file.path(generatedd,paste0("Measured_v_Calc_Weights_survey",
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

# 1. pull the commercial mean weight index generated in get-mean-weight.R
cmw <- readr::read_csv(here::here("data/generated/commercial_mean_weight_3CD.csv"))
# get the 2019 value in case we need it again
cmw2019 <- cmw %>%
  filter(year %in% 2019)

cmw <- cmw %>%
  rename(comm_mean_weight=mean_weight) %>%
  filter(!year %in% 2019) #remove 2019. It is based on 2 samples (n=360). Note is was used in the 2020 assessment, but probably shouldn't have been

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

 g <- tidyr::pivot_longer(dat1, cols = 2:3) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(year, value, colour = name)) +
    geom_vline(xintercept = 2000:2022, lty = 1, col = "grey80") +
    # geom_point(size=3.5) +
    geom_line(size=1.4) +
    ylim(0,3)+
    scale_color_aaas()+
    labs(title = Title, y = "Mean weight", x = "Year") +
   geom_point(data = dat1, mapping = aes(year, comm_mean_weight, size = n_samples), inherit.aes = FALSE, pch = 21, na.rm = TRUE) +
   scale_size_area(name = "Sampling events")

 g
 ggsave(file.path(generatedd,paste0("Comm_v_Survey_weights_",
                                 AREA,".png")))

# 4. Plot the two indices against each other (log space)
#    Note that the last pair of survey and commercial index values was in 2016
#    No survey in 2017, 2019 or 2020. No commercial samples in 2018.

r <- range(log(c(dat1$survey_mean_weight, dat1$comm_mean_weight)), na.rm = TRUE)

 g <- ggplot(dat1, aes(log(survey_mean_weight), log(comm_mean_weight))) +
    geom_point(aes(size = n_samples), pch = 21) +
    scale_size_area(name = "Sampling events", max_size = 10) +
    stat_smooth(method = "lm", se = FALSE)+
    ggrepel::geom_text_repel(aes(label = year), size = 4) +
    geom_abline(intercept = 0, slope = 1) +
   coord_equal() +
    #coord_fixed(xlim = c(r[1], r[2]), ylim = c(r[1], r[2])) +
    ylim(0,1.2)+xlim(0,1.2)+
    labs(title = paste(AREA, TYPE), x = "Ln survey mean weight", y = "Ln comm mean weight")
 g
 ggsave(file.path(generatedd,paste0("lnSurvey_v_lnCom_with_lm_fit_",
                                 AREA,".png")))
#
# ####################################################
# predict commercial mw from a glm
 nosurvyr <- c(2017,2019,2020)
 # 1. Fit glm
 GLM <- glm(comm_mean_weight ~ log(survey_mean_weight),
   family = Gamma(link = "log"),
   data = dat1)

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
pred_commercial_mean_weight <- predict(GLM, newdata, type="response")
#
comparedata <- newdata %>%
    cbind(pred_commercial_mean_weight)

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

  g1 <- comparedata_allyrs %>%
    select(-n_samples, -n_specimens) |>
    melt(id.vars="year", variable.name="Obs_vs_Pred", value.name="commercial_mean_weight") %>%
    ggplot()+
    geom_point(aes(x=year, y=commercial_mean_weight, colour=Obs_vs_Pred), size=2.5)+
    geom_line(aes(x=year, y=commercial_mean_weight, colour=Obs_vs_Pred), lwd=1, lty=1)+
    scale_color_aaas()+
    theme(legend.position = "right")+
    ylim(0,3.5)+
    scale_x_continuous(breaks=seq(min(comparedata_allyrs$year),max(comparedata_allyrs$year), by=2))+
    labs(title = paste(AREA, TYPE), y = "Mean weight", x = "Year")
  g1
  ggsave(file.path(generatedd,paste0("Compare_Obs_v_Predicted_Weight",
                                 AREA,".png")))

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
    points(x$year, x$pred_commercial_mean_weight, col = "darkgreen")
    lines(x$year, x$pred_commercial_mean_weight, col = "darkgreen")
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
    select(year,pred_commercial_mean_weight)
  pred_mean_weight_interpolate <- comparedata_allyrs %>%
    select(year,pred_commercial_mean_weight)

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


