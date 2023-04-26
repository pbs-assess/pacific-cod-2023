library(ggplot2)
library(dplyr)
library(readr)

dat <- readRDS(here::here("data/pcod-cache/pacific-cod.rds"))

compare_indices <- function(.survey, cpue_area, geo = FALSE) {
  surv <- dat$survey_index
  surv <- surv %>% rename(lwr = lowerci, upr = upperci, est = biomass) %>%
    filter(survey_abbrev %in% .survey) %>%
    select(year, lwr, upr, est) %>%
    mutate(type = "survey") %>%
    mutate(lwr = lwr / exp(mean(log(est)))) %>%
    mutate(upr = upr / exp(mean(log(est)))) %>%
    mutate(est = est / exp(mean(log(est))))

  cpue <- readr::read_csv(here::here("data/generated/cpue-predictions-modern.csv"),
    col_types = cols(
      formula = col_character(),
      area = col_character(),
      formula_version = col_character(),
      year = col_double(),
      model = col_character(),
      est_link = col_double(),
      se_link = col_double(),
      est = col_double(),
      lwr = col_double(),
      upr = col_double()
    ))

  cpue <- filter(cpue, formula == "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel) + (1 | year_locality)") %>%
    filter(area == cpue_area) %>%
    select(year, lwr, upr, est) %>%
    mutate(type = "cpue") %>%
    mutate(lwr = lwr / exp(mean(log(est)))) %>%
    mutate(upr = upr / exp(mean(log(est)))) %>%
    mutate(est = est / exp(mean(log(est)))) %>%
    filter(year >= min(surv$year)) %>%
    filter(year <= max(surv$year))


  # from sdmTMB:
  st <- readRDS(here::here("data/spt-index-out-pcod.rds")) %>%
    filter(survey == .survey) %>%
    select(year, lwr, upr, est) %>%
    mutate(type = "survey-geostat") %>%
    mutate(lwr = lwr / exp(mean(log(est)))) %>%
    mutate(upr = upr / exp(mean(log(est)))) %>%
    mutate(est = est / exp(mean(log(est))))

  d <- bind_rows(surv, cpue) %>%
    bind_rows(st)

  # d <- filter(d, type != "survey")
  if (!geo)
     d <- filter(d, type != "survey-geostat")
  if (geo)
     d <- filter(d, type != "survey")

  if (!geo) {
    x <- cpue$est[cpue$year %in% surv$year]
    y <- surv$est
  } else {
    if (geo)
      x <- cpue$est[cpue$year %in% st$year]
    y <- st$est
  }
  r <- cor.test(x, y)
  rest <- r[["estimate"]][["cor"]]
  p <- r[["p.value"]]

  g <- ggplot(d, aes(year, est, ymin = lwr, ymax = upr)) +
    geom_ribbon(aes(fill = type), alpha = 0.4) +
    geom_point(aes(colour = type), size = 3) +
    geom_line(aes(colour = type), lwd = 1.0) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    ylim(0, NA) +
    annotate("text", x = min(d$year), y = max(d$upr),
      label = paste0("r = ", round(rest, 2)), hjust = 0) +
    annotate("text", x = min(d$year), y = 0.95 * max(d$upr),
      label = paste0("p = ", round(p, 3)), hjust = 0) +
    xlab("") + ylab("Relative estimate") +
    labs(colour = "Type", fill = "Type") +
    theme(legend.position = c(0.9, 0.9)) +
    theme(panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(min(d$year), max(d$year)))
  g


}
#
# compare_indices("SYN QCS", "5ABCD")
# compare_indices("SYN HS", "5ABCD")
# compare_indices("SYN WCVI", "3CD")
#
# compare_indices("SYN QCS", "5ABCD", T)
# compare_indices("SYN HS", "5ABCD", T)
# compare_indices("SYN WCVI", "3CD", T)
