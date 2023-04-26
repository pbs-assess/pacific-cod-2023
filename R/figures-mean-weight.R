mw.plot <- function(model,
                    cv = 0.2,
                    every = 5,
                    last.yr = 2015,
                    french=FALSE){

  mpd <- model$mpd
  yrs <- model$dat$meanwtdata[,1]
  obs <- mpd$obs_annual_mean_weight
  fit <- mpd$annual_mean_weight

  i <- cbind(yrs, obs, fit) %>% as.tibble() %>%
    mutate(lower = obs - cv,
           upper = obs + cv)

  names(i) <- c("Year", "Annual mean weight (kg)", "Fit", "Lower", "Upper")

  p <- ggplot(i) +
    aes(x = Year,
        y = `Annual mean weight (kg)`) +
    geom_pointrange(aes(ymin = Lower,
                        ymax = Upper),
                    size = 0.25) +
    geom_line(color = "red",
              y = i$Fit,
              size = 1) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, last.yr, every))+
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Annual mean weight",translate=french, allow_missing = TRUE), "(kg)"))

  p
}

mw.compare.plot <- function(model,
                            french=FALSE){

  mpd <- model$mpd
  yrs <- model$dat$meanwtdata[,1]
  obs <- mpd$obs_annual_mean_weight
  fit <- mpd$annual_mean_weight

  dat <- as.tibble(data.frame(fit, obs))

  p <- ggplot(dat, aes(x = obs, y = fit)) +
    geom_point() +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    geom_abline(slope = 1,
                intercept = 0,
                color = "red",
                linetype = "dashed") +
    labs(x = paste(en2fr("Observed annual mean weight",translate=french, allow_missing = TRUE), "(kg)"),
         y = paste(en2fr("Estimated annual mean weight",translate=french, allow_missing = TRUE), "(kg)"))

  p
}
