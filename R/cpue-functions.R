make_bubble_dat <- function(dat,
  measure.vars = c("month", "locality_code", "vessel", "latitude", "depth")) {
  bubble_pos <- bind_rows(dat) %>%
    filter(spp_catch > 0) %>%
    reshape2::melt(id.vars = c("year", "area"),
      measure.vars = measure.vars) %>%
    group_by(area, year, variable, value) %>%
    summarize(n_pos = n())

  bubble_all <- bind_rows(dat) %>%
    reshape2::melt(id.vars = c("year", "area"),
      measure.vars = measure.vars) %>%
    group_by(area, year, variable, value) %>%
    summarize(n = n())

  left_join(bubble_all, bubble_pos, by = c("area", "year", "variable", "value")) %>%
    ungroup() %>%
    mutate(variable = gfplot:::firstup(gsub("_", " ", variable)))
}

make_facet_bubble_plot <- function(dat, group = "fishing events") {
  ggplot(dat, aes_string("as.factor(year)", y = "value")) +
    geom_point(aes_string(size = "n_pos", fill = "n"), alpha = 0.4, pch = 21) +
    geom_point(aes_string(size = "n"), alpha = 0.4, pch = 21) +
    facet_wrap(~variable, scales = "free", ncol = 2) +
    ggplot2::scale_x_discrete(breaks = seq(1950, 2020, 5)) +
    xlab("") + ylab("") +
    labs(size = paste0("Number of\n", group)) +
    labs(fill = paste0("Number of\n", group)) +
    ggplot2::scale_size_continuous(range = c(0, 7)) +
    ggplot2::scale_fill_viridis_c(trans = "log", breaks = c(1, 10, 100, 500)) +
    theme_pbs() +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
      legend.direction = "horizontal")
}

make_facet_bubble_plot_recent <- function(dat, group = "fishing events") {
  ggplot(dat, aes_string("as.factor(year)", y = "value")) +
    geom_point(aes_string(size = "n_pos", fill = "n"), alpha = 0.4, pch = 21) +
    geom_point(aes_string(size = "n"), alpha = 0.4, pch = 21) +
    facet_wrap(~variable, scales = "free", ncol = 2) +
    ggplot2::scale_x_discrete(breaks = seq(2010, 2020, 1)) +
    xlab("") + ylab("") +
    labs(size = paste0("Number of\n", group)) +
    labs(fill = paste0("Number of\n", group)) +
    ggplot2::scale_size_continuous(range = c(0, 7)) +
    ggplot2::scale_fill_viridis_c(trans = "log", breaks = c(1, 10, 100, 500)) +
    theme_pbs() +
    theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0),
          legend.direction = "horizontal")
}

make_cpue_ts_dat <- function(dat) {
  pp <- dat %>%
    group_by(formula_version, model, area) %>%
    mutate(geo_mean = exp(mean(log(est)))) %>%
    mutate(upr = upr / geo_mean, lwr = lwr / geo_mean, est = est / geo_mean) %>%
    ungroup() %>%
    mutate(formula_version =
        gsub("Full standardization minus interactions",
          "Full standardization\nminus interactions", formula_version)) %>%
    mutate(formula_version =
        forcats::fct_relevel(formula_version,
          "Full standardization\nminus interactions", after = Inf)) %>%
    mutate(formula_version =
        forcats::fct_relevel(formula_version,
          "Full standardization", after = Inf))
  uns <- filter(pp, formula_version == "Unstandardized") %>% select(-formula_version)
  list(stand = pp, unstand = uns)
}

make_cpue_ts_plot <- function(dat) {
# First need to rename formula_version values if french.
 if(french==TRUE){
   dat$stand$formula_version <- as.character(dat$stand$formula_version) #replace doesn't work with factors
   dat$stand <- dat$stand %>%
      mutate(formula_version = replace(formula_version,formula_version == "Month", "Mois")) %>%
      mutate(formula_version = replace(formula_version,formula_version == "Locality", "Lieu"))%>%
      mutate(formula_version = replace(formula_version,formula_version == "Depth", "Profondeur"))%>%
      mutate(formula_version = replace(formula_version,formula_version == "Vessel", "Navire"))%>%
      mutate(formula_version = replace(formula_version,formula_version == "Unstandardized", "Non normalisée"))%>%
      mutate(formula_version = replace(formula_version,formula_version == "Full standardization", "Normalisation\ncomplet"))%>%
      mutate(formula_version =
               replace(formula_version,formula_version == "Full standardization\nminus interactions", "Normalisation\ncomplet\nmois interactions")) %>%

     filter(formula_version == "Normalisation\ncomplet")
#english
  } else {
    dat$stand <- dat$stand %>%
    #filter(formula_version != "Unstandardized")
    filter(formula_version == "Full standardization")
  }

dat$stand %>%
   ggplot(aes(year, est, ymin = lwr, ymax = upr, fill = formula_version)) +
    geom_line() +
    geom_ribbon(data = dat$unstand, aes(x = year, ymin = lwr, ymax = upr),
      fill = "black", alpha = 0.5,
      inherit.aes = FALSE) +
    geom_line(data = dat$unstand, aes(x = year, y = est),
      colour = "black", inherit.aes = FALSE) +
    labs(fill = "Version", colour = "Version", y = "") +
    geom_ribbon(alpha = 0.5) +
    geom_line(aes(colour = formula_version)) +
    facet_grid(area~formula_version) +
    ylab("CPUE (kg/hour) divided\nby geometric mean") +
    guides(fill = FALSE, colour = FALSE) + xlab("")
}



make_cpue_ts_dat_noint <- function(dat) {
  pp <- dat %>%
    group_by(formula_version, model, area) %>%
    mutate(geo_mean = exp(mean(log(est)))) %>%
    mutate(upr = upr / geo_mean, lwr = lwr / geo_mean, est = est / geo_mean) %>%
    ungroup() %>%
    mutate(formula_version =
        gsub("Full standardization minus interactions",
          "Full standardization\nminus interactions", formula_version)) %>%
    mutate(formula_version =
        forcats::fct_relevel(formula_version,
          "Full standardization\nminus interactions", after = Inf)) %>%
    mutate(formula_version =
        forcats::fct_relevel(formula_version,
          "Full standardization", after = Inf))
  uns <- filter(pp, formula_version == "Full standardization\nminus interactions") %>%
    select(-formula_version)
  list(stand = pp, unstand = uns)
}

make_cpue_ts_plot_noint <- function(dat) {
  dat$stand %>%
    filter(formula_version == "Full standardization") %>%
    ggplot(aes(year, est, ymin = lwr, ymax = upr, fill = formula_version)) +
    geom_line() +
    geom_ribbon(data = dat$unstand, aes(x = year, ymin = lwr, ymax = upr),
      fill = "black", alpha = 0.5,
      inherit.aes = FALSE) +
    geom_line(data = dat$unstand, aes(x = year, y = est),
      colour = "black", inherit.aes = FALSE) +
    labs(fill = "Version", colour = "Version", y = "") +
    geom_ribbon(alpha = 0.5) +
    geom_line(aes(colour = formula_version)) +
    facet_wrap(~area, ncol = 2) +
    ylab("CPUE (kg/hour) divided\nby geometric mean") +
    guides(fill = FALSE, colour = FALSE) + xlab("") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")
}

make_fe_plots <- function(object) {
  invisible(capture.output(
    su <- summary(object)$coefficients$cond
  ))
  sud <- as.data.frame(su)
  sud$param <- row.names(su)
  row.names(sud) <- NULL
  sud <- rename(sud, est = Estimate, se = `Std. Error`)
  sud <- mutate(sud, par_value = gsub("^[A-Z_a-z]+", "", param))
  sud <- mutate(sud, par_group = gsub("^([A-Z_a-z]+)[0-9.]+$", "\\1", param))

  if(french==TRUE){
    sud <- sud %>%
      mutate(par_group = replace(par_group,par_group == "month", "mois")) %>%
      mutate(par_group = replace(par_group,par_group == "depth", "profondeur")) %>%
      mutate(par_group = replace(par_group,par_group == "year_factor", "annee"))
    }


  ggplot(sud, aes_string("est", "forcats::fct_rev(par_value)",
    yend = "forcats::fct_rev(par_value)"
  )) +
    ggplot2::geom_segment(aes_string(
      x = "est - 1.96 * se",
      xend = "est + 1.96 * se"
    ), lwd = 0.5) +
    ggplot2::geom_segment(aes_string(
      x = "est - 0.67 * se",
      xend = "est + 0.67 * se"
    ), lwd = 1.25) +
    geom_point() +
    facet_wrap(~par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    labs(x = "Coefficient value (log space)", y = "Predictor value")
}
make_re_dat <- function(object) {
  re <- glmmTMB::ranef(object)
  plyr::ldply(re$cond, function(x) {
    sud <- as.data.frame(x)
    sud$par_value <- row.names(sud)
    row.names(sud) <- NULL
    sud
  }) %>%
    rename(par_group = .id) %>%
    rename(est = `(Intercept)`) %>%
    as_tibble() %>%
    mutate(loc_group = gsub("^([0-9]+)[ -]*([0-9a-zA-Z-]+)$", "\\2", par_value)) %>%
    mutate(loc_year = gsub("^([0-9]+)[ -]*[0-9a-zA-Z-]+$", "\\1", par_value))
}

make_re_plots <- function(object, re_names = c("locality")) {
  re <- make_re_dat(object)

  if(french==TRUE){
    re <- re %>%
      mutate(par_group = replace(par_group,par_group == paste(re_names), paste(en2fr(paste(re_names), translate=french, allow_missing=T, case="lower"))))
    re_names = paste(en2fr(paste(re_names), translate=french, allow_missing=T, case="lower"))
  }

  filter(re, par_group %in% re_names) %>%
    ggplot(aes_string("est", "forcats::fct_rev(par_value)")) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.4) +
    geom_point(bg = "white") +
    facet_wrap(~par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    labs(x = "Random intercept value (log space)", y = "")
}

make_year_locality_plots <- function(object) {
  re <- make_re_dat(object)
  filter(re, par_group == "year_locality") %>%
    ggplot(aes_string("as.numeric(loc_year)", "est", group = "loc_group")) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.4) +
    geom_point(alpha = 0.7) +
    geom_line(alpha = 0.3) +
    facet_wrap(~loc_group) +
    scale_x_continuous(breaks = seq(1900, 3000, 10)) +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    labs(x = "Year", y = "Random intercept\n(interaction) value (log space)")
}

most_common <- function(x) names(rev(sort(table(as.character(x)))))[[1]]
basel_level <- function(x) levels(x)[[1]]

plot_cpue_int_res <- function(model, fleet, index_data,
  era = c("modern", "historical"), ref_type = c("base", "common"),
  the_formula_version = "Full standardization") {

  era <- match.arg(era)
  ref_type <- match.arg(ref_type)

  if (ref_type == "base") {
    newdata <- select(fleet, year_factor, locality) %>%
      unique() %>%
      mutate(year = as.numeric(as.character(year_factor))) %>%
      group_by(locality) %>%
      do({
        data_frame(year = seq(min(.$year), max(.$year)))
      }) %>%
      group_by(locality) %>%
      mutate(
        depth = basel_level(fleet$depth),
        month = basel_level(fleet$month)
      )
    if (era == "modern") {
      newdata <- newdata %>% mutate(
        latitude = basel_level(fleet$latitude),
        vessel = basel_level(fleet$vessel))
    }
  } else {
    stop("Commented out for simplicity; needs to be checked")
    ##    if (era != "modern") {
    ##    newdata <- fleet %>%
    ##      mutate(year = as.numeric(as.character(year_factor))) %>%
    ##      group_by(locality) %>%
    ##      summarise(
    ##        year_min = min(year),
    ##        year_max = max(year),
    ##        depth = most_common(depth),
    ##        month = most_common(month)
    ##      )
    ##    }
    ##    if (era == "modern") {
    ##      newdata <- fleet %>%
    ##      mutate(year = as.numeric(as.character(year_factor))) %>%
    ##      group_by(locality) %>%
    ##      summarise(
    ##        year_min = min(year),
    ##        year_max = max(year),
    ##        depth = most_common(depth),
    ##        month = most_common(month),
    ##        latitude = most_common(latitude),
    ##        vessel = most_common(vessel)
    ##      )
    ##    }
    ##    if (era != "modern") {
    ##      newdata <- newdata %>% group_by(locality) %>%
    ##        do({
    ##          data_frame(
    ##            year = seq(.$year_min, .$year_max),
    ##            depth = .$depth,
    ##            month = .$month
    ##          )
    ##        })
    ##    }
    ##    if (era == "modern") {
    ##      newdata <- newdata %>% group_by(locality) %>%
    ##        do({
    ##          data_frame(
    ##            year = seq(.$year_min, .$year_max),
    ##            depth = .$depth,
    ##            month = .$month,
    ##            vessel = .$vessel,
    ##            latitude = .$latitude
    ##          )
    ##        })
    ##    }
  }
  newdata <- ungroup(newdata)
  newdata <- newdata %>% mutate(year_locality = paste(year, locality))
  newdata <- newdata %>% mutate(year_factor = as.character(year))

  invisible(capture.output({
    pp <- predict(model, newdata = newdata)
  }))
  newdata$loc_pred <- pp
  newdata <- arrange(newdata, year, locality)

  stand <- filter(index_data, formula_version == the_formula_version,
    area == unique(fleet$area)[[1]]) %>%
    select(year, est_link, se_link)

  ggplot(newdata, aes(year, loc_pred)) +
    geom_line(alpha = 0.7, aes(group = locality, colour = locality)) +
    geom_line(data = stand, aes(x = year, est_link), inherit.aes = FALSE, lwd = 1) +
    geom_ribbon(data = stand,
      aes(x = year, ymin = est_link - 1.96 * se_link,
        ymax = est_link + 1.96 * se_link), inherit.aes = FALSE, alpha = 0.4) +
    guides(colour = FALSE) +
    scale_color_discrete() + ylab("Log of standardized CPUE") +
    xlab("")
}

make_catch_effort_ts_plot <- function(dat) {
  bind_rows(dat) %>% group_by(year, area) %>%
    summarise(
      `Catch` = sum(spp_catch)/1000,
       `Hours` = sum(hours_fished)/1000) %>%
    reshape2::melt(id.vars = c("year", "area")) %>%
    ggplot(aes(year, value)) +
    geom_line() +
    facet_grid(variable~area, scales = "free_y") +
    ylab(paste(en2fr("Value",translate=french,allow_missing=TRUE), "(1000 kg", en2fr("or",translate=french,allow_missing=TRUE, case ="lower"), "1000 hours)")) + xlab("") +
    ylim(0, NA)
}

make_catch_effort_ts_plot_modern <- function(dat) {
  bind_rows(dat) %>% group_by(year, area) %>%
    summarise(
      `Catch` = sum(spp_catch)/1000,
      `Hours` = sum(hours_fished)/1000) %>%
    reshape2::melt(id.vars = c("year", "area")) %>%
    ggplot(aes(year, value)) +
    geom_line() +
    facet_grid(variable~area, scales = "free_y") +
    ylab(paste(en2fr("Value",translate=french,allow_missing=TRUE), "(1000 kg", en2fr("or",translate=french,allow_missing=TRUE, case ="lower"), "1000 hours)")) +
    xlab("") +
    ylim(0, NA)
}

make_catch_effort_ts_plot_fr <- function(dat) {
  bind_rows(dat) %>% group_by(year, area) %>%
    summarise(
      `Prise` = sum(spp_catch)/1000,
      `Heures` = sum(hours_fished)/1000) %>%
    reshape2::melt(id.vars = c("year", "area")) %>%
    ggplot(aes(year, value)) +
    geom_line() +
    facet_grid(variable~area, scales = "free_y") +
    ylab(paste(en2fr("Value",translate=french,allow_missing=TRUE), "(1000 kg", en2fr("or",translate=french,allow_missing=TRUE, case ="lower"), "1000", en2fr("hours",translate=french,allow_missing=TRUE, case ="lower"),")")) +
    xlab("") +
    ylim(0, NA)
}

make_catch_effort_ts_plot_modern_fr <- function(dat) {
  bind_rows(dat) %>% group_by(year, area) %>%
    summarise(
      `Prise` = sum(spp_catch)/1000,
      `Heures` = sum(hours_fished)/1000) %>%
    reshape2::melt(id.vars = c("year", "area")) %>%
    ggplot(aes(year, value)) +
    geom_line() +
    facet_grid(variable~area, scales = "free_y") +
    ylab(paste(en2fr("Value",translate=french,allow_missing=TRUE), "(1000 kg", en2fr("or",translate=french,allow_missing=TRUE, case ="lower"), "1000", en2fr("hours",translate=french,allow_missing=TRUE, case ="lower"),")")) +
    xlab("") +
    ylim(0, NA)
}


