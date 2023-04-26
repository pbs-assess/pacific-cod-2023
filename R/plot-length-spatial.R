## Map the length samples
## Modified from plot_cpue_spatial in gfplot

# must supply the data list dat and the years
plot_length_spatial <- function (dat,
                                 years,
                                 survey=c("SYN WCVI"),
                                 bin_width = 5,
                                 utm_zone = 9, bath = c(100,200, 500),
                                 xlim = c(500, 890), ylim = c(5350, 5650),
                                 fill_scale = ggplot2::scale_fill_viridis_c(trans = "sqrt", option = "D"),
                                 colour_scale = ggplot2::scale_colour_viridis_c(trans = "sqrt", option = "D"),
                                 fill_lab = "Length (cm)",
                                 min_cells = 1)
{

  # filter set data for area and remove extraneous fields
  setdat <- dat$survey_sets %>%
    dplyr::filter(survey_abbrev %in% survey, year %in% years) %>%
    dplyr::select(year,survey_id,fishing_event_id,latitude,longitude,depth_m)

  # filter sample data for area and maturity and join to add lat-longs
  suppressMessages(sampdat <- dat$survey_samples %>%
                     dplyr::filter(survey_abbrev %in% survey,
                                   !is.na(length),
                                   year %in% years) %>%
                     left_join(setdat) %>%
                     dplyr::filter(!is.na(latitude),!is.na(longitude)) %>%
                     rename(X = longitude, Y = latitude) %>%
                     dplyr::select(year,survey_abbrev,fishing_event_id,Y,X,depth_m,specimen_id, length))

  sampdat <- ll2utm(sampdat, utm_zone = utm_zone)

  llrange <- utm2ll(cbind(X = xlim, Y = ylim))
  coastline_utm <- gfplot:::load_coastline(llrange$X + c(-1, 1),
                                   llrange$Y + c(-1, 1),
                                   utm_zone = utm_zone)
  isobath_utm <- gfplot:::load_isobath(llrange$X + c(-1, 1),
                                       llrange$Y + c(-1, 1),
                                       bath=bath,
                                       utm_zone = utm_zone)


  # bin_length_data() is below. Adapted from enact_privacy_rule in gfplot
  data_out <- bin_length_data(sampdat, bin_width = bin_width,
                              xlim = xlim, ylim = ylim)
  gdat <- data_out$data
  hex_dat <- compute_hexagon_xy(gdat, bin_width = bin_width)

  g <- ggplot()+
    geom_polygon(data = hex_dat,
                aes_string(x = "X", y = "Y",
               fill = "length", colour = "length", group = "hex_id"),
               inherit.aes = FALSE, lwd = 0.2) +
    geom_polygon(data = coastline_utm,
                   aes_string(x = "X",y = "Y", group = "PID"),
                     inherit.aes = FALSE,
                     lwd = 0.2, fill = "grey90", col = "grey70") +
    coord_equal(xlim = xlim, ylim = ylim) +
    geom_path(data = isobath_utm,
                   aes_string(x = "X",y = "Y",group = "paste(PID, SID)"),
                   inherit.aes = FALSE,
                   lwd = 0.4, col = "grey70") +

    fill_scale + colour_scale +
    facet_wrap(~year)+
    theme_pbs() +
    labs(fill = fill_lab, colour = fill_lab, y = "Northing", x = "Easting")+
    scale_fill_viridis_c(trans = "sqrt")
    g

}

plot_comm_sets <- function(dat,
                           min_year,
                           bydate = TRUE,
                           utm_zone = 9,
                           bath = c(100,200, 500),
                           xlim = c(500, 890), ylim = c(5350, 5650)) {

  dd <- dat %>%
    rename(X = Best_Long, Y = Best_Lat) %>%
    mutate(X=-X,
           SAMPLE_DATE=as.character(SAMPLE_DATE)) %>%
    filter(year>=min_year) %>%
    select(year,SAMPLE_DATE,X,Y,N_Lengths)

  dd <- ll2utm(dd, utm_zone = utm_zone)

  llrange <- utm2ll(cbind(X = xlim, Y = ylim))

  coast <- gfplot:::load_coastline(llrange$X + c(-1, 1),
                                           llrange$Y + c(-1, 1),
                                           utm_zone = utm_zone)
  if(bydate==TRUE){
    ggplot(dd, aes(X, Y))+
      geom_point(data = dd, pch = 4,
                 size = 2, col = "red") +
      coord_equal(expand = FALSE, xlim = xlim, ylim = ylim) +
      geom_point(pch = 21,size = 2, col = "red") +
      facet_wrap(~SAMPLE_DATE) +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE) +
      scale_fill_viridis_c(trans = "sqrt") +
      labs(x = "Easting",
           y = "Northing") +
      gfplot::theme_pbs()+
      guides(
        size = guide_legend(order = 1),
        fill = guide_colorbar(order = 0))
  }else{
    ggplot(dd, aes(X, Y))+
      geom_point(data = dd, pch = 4,
                 size = 2, col = "red") +
      coord_equal(expand = FALSE, xlim = xlim, ylim = ylim) +
      geom_point(pch = 21,size = 2, col = "red") +
      facet_wrap(~year) +
      geom_polygon(
        data = coast, aes_string(x = "X", y = "Y", group = "PID"),
        fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE) +
      scale_fill_viridis_c(trans = "sqrt") +
      labs(x = "Easting",
           y = "Northing") +
      gfplot::theme_pbs()+
      guides(
        size = guide_legend(order = 1),
        fill = guide_colorbar(order = 0))
  }
}


# Helper functions from gfplot/R/cpue-map.R

hex_coords <- function(x, y, unitcell_x = 1, unitcell_y = 1) {
  data.frame(
    x = hexbin::hexcoords(unitcell_x)$x + x,
    y = hexbin::hexcoords(unitcell_y)$y + y
  )
}

utm2ll <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "UTM"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

ll2utm <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "LL"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

load_coastline <- function(xlim_ll, ylim_ll, utm_zone, buffer = 2) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
                              xlim = xlim_ll + c(-buffer, buffer),
                              ylim = ylim_ll + c(-buffer, buffer)
  )
  ll2utm(np, utm_zone = utm_zone)
}

load_isobath <- function(xlim_ll, ylim_ll, bath, utm_zone) {
  data("isobath", package = "PBSdata", envir = environment())
  isobath <- filter(isobath, .data$PID %in% bath)
  isobath <- PBSmapping::clipPolys(isobath,
                                   xlim = xlim_ll + c(-3, 3),
                                   ylim = ylim_ll + c(-3, 3)
  )
  ll2utm(isobath, utm_zone = utm_zone)
}

compute_hexagon_xy <- function(gdat, bin_width) {
  # compute hexagon x-y coordinates for geom_polygon()
  dx <- bin_width/2
  dy <- bin_width/2

  # RF Need to loop this over years too :(
  public_daty <- data.frame()
  yrs <- unique(gdat$year)

  for(ii in yrs){
    gdaty <- gdat %>%
      dplyr::filter(year == ii)

    public_daty <- lapply(seq_len(nrow(gdaty)), function(i)
      data.frame(
        hex_id = i, length = gdaty[i, "value"],
        hex_coords(gdaty[i, "x"], gdaty[i, "y"], dx, dy)
      )) %>%
      bind_rows()%>%
      mutate(year=ii)


    public_daty$X <- public_daty$x
    public_daty$Y <- public_daty$y

    if(ii==yrs[1]){
      public_dat <- public_daty
    }else public_dat <- rbind(public_dat,public_daty)

  }# end for

  public_dat
}# end function

# adapted from enact_privacy_rule() in gfplot/R/cpue-map.R
# RF March 31 2022
bin_length_data <- function(sdat, bin_width, xlim, ylim) {
  # count unique vessels per hexagon cell for privacy:

  # Fake data to make sure that the hexagons overlap perfectly.
  # This extends the X and Y to extreme but identical limits every time.
  fake_rows <- sdat[1:2, , drop = FALSE]
  fake_rows$fishing_event_id <- c(-999L, -998L)
  fake_rows$X <- c(-1000, 20000)
  fake_rows$Y <- c(-1000, 20000)
  dat <- bind_rows(sdat, fake_rows)

  # the actual length hexagon binning:
  # RF Need to filter by year
  gdat_y <- data.frame()
  yrs <- unique(sdat$year)

  for(ii in yrs){
    sdaty <- sdat %>%
      dplyr::filter(year == ii)

    g <- ggplot(sdaty, aes_string("X", "Y")) +
      coord_equal(xlim = xlim, ylim = ylim) +
      stat_summary_hex(
        aes_string(x = "X", y = "Y", z = "length"),
        data = sdaty, binwidth = bin_width,
        fun = function(x) exp(mean(log(x), na.rm = FALSE)))

    gdat <- ggplot2::ggplot_build(g)$data[[1]] %>%
      mutate(year=ii)


    if(ii==yrs[1]){
      gdat_y <- gdat
    }else gdat_y <- rbind(gdat_y,gdat)

  } # end for

  list(data = gdat_y)
} # end function

