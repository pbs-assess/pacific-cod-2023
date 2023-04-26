gg_cpue <- list()
spp <- "pcod"

if (params$era == "modern") {
  fi <- here::here("data/generated/cpue-modern.rds")
  if (!file.exists(fi)) {
    d1996 <- gfplot::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
    readr::write_rds(d1996, fi)
  } else {
    d1996 <- readr::read_rds(fi)
  }
} else {
  fi <- here::here("data/generated/cpue-historic.rds")
  if (!file.exists(fi)) {
    d <- gfplot::get_cpue_historical(species = NULL, end_year = 1995,
      alt_year_start_date = "04-01")
    readr::write_rds(d, fi)
  } else {
    d <- readr::read_rds(fi)
  }
}

if (params$era == "modern") {
  define_fleet <- function(area, area_name) {
    # d1996$catch_kg <- d1996$landed_kg + d1996$discarded_kg
    # d1996 <- mutate(d1996, species_common_name = ifelse(species_code == 222, "pacific cod", "ignore me"))
    out <- tidy_cpue_index(d1996,
      species_common = tolower(params$species_proper),
      gear = "bottom trawl",
      alt_year_start_date = "04-01",
      use_alt_year = params$april1_year,
      year_range = c(1996, 2019),
      lat_range = c(48, Inf),
      min_positive_tows = 100,
      min_positive_trips = 5,
      min_yrs_with_trips = 5,
      depth_band_width = 25,
      area_grep_pattern = area,
      depth_bin_quantiles = c(0.001, 0.999),
      min_bin_prop = 0.001,
      lat_band_width = 0.1)
    out$area <- area_name
    out
  }
  dfleet <- purrr::map2(params$area, params$area_name, define_fleet)
} else {
  define_fleet <- function(area, area_name) {
    out <- tidy_cpue_historical(d,
      species_common = tolower(params$species_proper),
      use_alt_year = params$april1_year,
      year_range = c(1956, 1995),
      depth_band_width = 25,
      area_grep_pattern = area,
      depth_bin_quantiles = c(0.001, 0.999),
      min_bin_prop = 0.001)
    out$area <- area_name
    out
  }
  dfleet <- purrr::map2(params$area, params$area_name, define_fleet)
}

depth_bands <- as.numeric(as.character(unique(bind_rows(dfleet)$depth)))

gg_cpue$depth <- dfleet %>%
  bind_rows() %>%
  mutate(`Trip or fishing event\ncaught this species` =
      ifelse(pos_catch == 1, "Yes", "No")) %>%
  ggplot(aes(best_depth, fill = `Trip or fishing event\ncaught this species`)) +
  geom_histogram(binwidth = 10) +
  ylim(0, NA) +
  geom_vline(xintercept = depth_bands, lty = 2, col = "grey80") +
  coord_cartesian(expand = FALSE) +
  facet_wrap(~area, ncol = 2)

for (i in seq_along(dfleet)) {
  dfleet[[i]]$year_locality <- paste(dfleet[[i]]$year_factor, dfleet[[i]]$locality)
}

if (params$era == "modern") {
  formulas <- tibble::tibble(
    formula = c(
      "cpue ~ 0 + year_factor",
      "cpue ~ 0 + year_factor + depth",
      "cpue ~ 0 + year_factor + month",
      "cpue ~ 0 + year_factor + latitude",
      "cpue ~ 0 + year_factor + (1 | locality)",
      "cpue ~ 0 + year_factor + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel) + (1 | year_locality)"
    ),
    formula_version = c(
      "Unstandardized",
      "Depth",
      "Month",
      "Latitude",
      "Locality",
      "Vessel",
      "Full standardization minus interactions",
      "Full standardization"
    )
  )
} else {
  formulas <- tibble::tibble(
    formula = c(
      "cpue ~ 0 + year_factor",
      "cpue ~ 0 + year_factor + depth",
      "cpue ~ 0 + year_factor + month",
      "cpue ~ 0 + year_factor + (1 | locality)",
      "cpue ~ 0 + year_factor + depth + month + (1 | locality)",
      "cpue ~ 0 + year_factor + depth + month + (1 | locality) + (1 | year_locality)"
    ),
    formula_version = c(
      "Unstandardized",
      "Depth",
      "Month",
      "Locality",
      "Full standardization minus interactions",
      "Full standardization"
    )
  )
}

torun <- expand.grid(formula = formulas$formula,
  area = params$area_name, stringsAsFactors = FALSE)
torun <- inner_join(torun, formulas, by = "formula")

if (params$skip_single_variable_models) {
  torun <- filter(torun,
    formula_version %in% c("Unstandardized", "Full standardization minus interactions",
      "Full standardization"))
}

file_model <- here::here(paste0("data/generated/cpue-models-",
  spp, "-", params$era, ".rds"))

if (!file.exists(file_model)) {
  invisible(capture.output({
    model <- plyr::mlply(torun, function(formula, area, formula_version) {
      df <- dfleet[[which(params$area_name == area)]]
      message("Fitting area ", area, " and model ", formula)
      fit_cpue_index_glmmtmb(df, as.formula(formula))
    })
  }))
  saveRDS(model, file_model)
} else {
  model <- readRDS(file_model)
}

predictions <- plyr::ldply(model, predict_cpue_index_tweedie)
readr::write_csv(predictions,
  here::here(paste0("data/generated/cpue-predictions-", spp, "-", params$era, ".csv")))

for (i in seq_along(dfleet)) {
   if ("hours_fished" %in% names(dfleet[[i]])) {
     dfleet[[i]] <- dplyr::rename(dfleet[[i]], effort = hours_fished)
   }
 }

arith_cpue <- dfleet %>%
  bind_rows() %>%
  group_by(area, year) %>%
  summarise(est = sum(spp_catch) / sum(effort)) %>%
  mutate(model = "Combined") %>%
  group_by(area) %>%
  mutate(geo_mean = exp(mean(log(est)))) %>%
  mutate(est = est/geo_mean) %>%
  ungroup()

gg_cpue$pred <- predictions %>%
  filter(formula_version %in% c("Unstandardized", "Full standardization")) %>%
  gfplot:::plot_cpue_predictions("Combined", scale = TRUE) +
  geom_line(data = arith_cpue, aes(year, est),
    inherit.aes = FALSE, lty = 2) +
  scale_x_continuous(breaks = seq(1950, 2050, 5))

gg_cpue$pred

