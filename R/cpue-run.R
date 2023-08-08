# Code to make the CPUE figures, moved from 03-figures.Rmd
#     so that the CPUE files don't need to be loaded each time

if(!file.exists(file.path(generatedd,paste0("CPUE_All_Fully_Standardized_3CD.png")))) {
  params <- list()
  params$species_proper <- "Pacific Cod"
  params$april1_year <- TRUE
  params$area <- c("3[CD]+")
  params$area_name <- c("3CD")
  params$skip_single_variable_models <- FALSE

  params$era <- "historic"
  source(here::here("R/cpue.R"))
  dfleet_hist <- dfleet
  gg_cpue_hist <- gg_cpue
  cpue_pred_hist <- predictions
  arith_cpue_hist <- arith_cpue
  m_historic <- readRDS(here::here("data/generated/cpue-models-pcod-historic.rds"))


  params$era <- "modern"
  source(here::here("R/cpue.R"))
  dfleet_modern <- dfleet
  gg_cpue_modern <- gg_cpue
  cpue_pred_modern <- predictions
  arith_cpue_modern <- arith_cpue
  m_modern <- readRDS(here::here("data/generated/cpue-models-pcod-modern.rds"))

  readr::write_csv(cpue_pred_modern, here::here("data/generated/cpue-predictions-modern.csv"))
  readr::write_csv(cpue_pred_hist, here::here("data/generated/cpue-predictions-historical.csv"))

  # Make figure
  g1 <- make_cpue_ts_dat(cpue_pred_hist) %>% make_cpue_ts_plot() +
    #ggtitle("1956-1995 CPUE") +
    #ylab("")+
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  g2 <- make_cpue_ts_dat(cpue_pred_modern) %>% make_cpue_ts_plot() +
    #ggtitle("1996+ CPUE") +
    #ylab("")+
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  P <- cowplot::plot_grid(g1,g2, ncol=1,labels = c("",""), greedy=TRUE)
  y.grob <- textGrob(en2fr("CPUE (kg/hour)",translate=french),
                     gp=gpar(fontface="bold", fontsize=15), rot=90)
  x.grob <- textGrob(en2fr("Year",translate=french),
                     gp=gpar(fontface="bold", fontsize=15))
  grid.arrange(arrangeGrob(P, left = y.grob, bottom = x.grob))

  # write to file
  ggsave(file.path(generatedd,paste0("CPUE_All_Fully_Standardized_3CD.png")))

} #end if
