plot_multiyear_survey_sets <- function(dat,
                                      survey_abbrev,
                                      density_column = "density_kgpm2",
                                      density_lab = expression(Density~(kg/km^2)),
                                      density_multiplier = 1e6) {
  dd <- gfplot::tidy_survey_sets(dat, survey_abbrev, years = seq(0, 1e6),
    density_column = density_column)
  coast <- gfplot:::load_coastline(range(dd$lon) + c(-1, 1),
    range(dd$lat) + c(-1, 1),
    utm_zone = 9
  )
  ggplot(filter(dd, present == 1), aes(X, Y,
    size = density*density_multiplier,
    fill = density*density_multiplier)) +
    scale_size_area(max_size = 8) +
    geom_point(data = filter(dd, present == 0), pch = 4,
      size = 1, col = "grey60") +
    coord_equal(expand = FALSE, xlim = range(dd$X), ylim = range(dd$Y)) +
    geom_point(pch = 21) +
    facet_wrap(~year) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE) +
    scale_fill_viridis_c(trans = "sqrt") +
    labs(fill = density_lab,
      size = density_lab, x = "Easting",
      y = "Northing") +
    guides(
      size = guide_legend(order = 1),
      fill = guide_colorbar(order = 0))
}
