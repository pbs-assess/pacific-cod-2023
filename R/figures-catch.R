make.catches.plot <- function(dat,
                              every = 5,
                              last.yr = 2022,
                              french=FALSE){
  dat <- dat %>%
    select(-total_catch) %>%
    group_by(year) %>%
    summarize(usa_catch = sum(usa_catch),
              canada_catch = sum(canada_catch))
  dat <- melt(dat, id.vars = "year")
  p <- ggplot(dat) +
    aes(x = year, y = value, fill = variable) +
    geom_col() +
    coord_cartesian(expand = FALSE) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Catch",translate=french, allow_missing = TRUE),"(t)"),
         fill = "") +
    scale_fill_brewer(labels = c(en2fr("USA",translate=french, allow_missing = TRUE), en2fr("Canada",translate=french, allow_missing = TRUE)), palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, last.yr, every)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  p
}

discards.plot <- function(dat,
                          french=FALSE){
  dat <- dat %>%
    group_by(year) %>%
    summarize(`Released at sea` = sum(discarded_canada) ,
              `Prop. released` = sum(discarded_canada) / sum(landed_canada + discarded_canada)) %>%
    rename(Year = year)

  g.bottom <- ggplot(dat) +
    aes(x = Year, y = `Released at sea`) +
    geom_col(fill = RColorBrewer::brewer.pal(3, "Dark2")[[2]]) +
    coord_cartesian(expand = FALSE) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Catch",translate=french, allow_missing = TRUE),"(t)"),
         fill = "") +
    scale_x_continuous(breaks = seq(0, 2020, 5))

  if(french==TRUE) {
    ylab <- paste("Prop.", en2fr("discarded",translate=french, allow_missing = TRUE))
  } else {
    ylab <- "Prop. released"
   }

  g.top <- ggplot(dat) +
    aes(x=Year, y= `Prop. released`)+
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
        y = ylab) +
    geom_line(color = "grey50",
              size = 1,
              alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 2022, 5))


  grid.arrange(g.top, g.bottom, heights = c(1/3, 2/3))

}

catch.fit.plot <- function(model,
                           every = 5,
                           last.yr = 2022,
                           french=FALSE){

  model <- model[[1]]
  obs <- model$dat$catch %>%
    as.tibble() %>%
    select(year, value)
  fit <- model$mpd$ct

  i <- as.tibble(cbind(obs, fit))

  names(i) <- c("Year", "Catch (t)", "Fit")

  p <- ggplot(i) +
    aes(x = Year,
        y = `Catch (t)`) +
    geom_point(size = 2) +
    geom_line(color = "red",
              y = i$Fit,
              size = 1) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Catch",translate=french, allow_missing = TRUE),"(t)"),
         fill = "") +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, last.yr, every))
  p
}


make.catches.plot.gear <- function(dat,
                                   every = 5,
                                   last.yr = 2022,
                                   french=FALSE){
  dat <- dat %>%
    select(-total_catch) %>%
    group_by(year, gear) %>%
    summarize(catch_weight = sum(catch_weight))
  dat <- melt(dat, id.vars = c("year", "gear"))
  p <- ggplot(dat) +
    aes(x = year, y = value, fill = gear) +
    geom_col() +
    coord_cartesian(expand = FALSE) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Catch",translate=french, allow_missing = TRUE),"(t)"),
         fill = "")+
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, last.yr, every)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(4.6, 1),
          legend.title = element_blank())
  p
}

make.catches.plot.vessel <- function(dat,
                                     every = 5,
                                     last.yr = 2022,
                                     french=FALSE){
  dat <- dat %>%
    select(-total_catch) %>%
    group_by(year, vessel_name) %>%
    summarize(catch_weight = sum(catch_weight))
  dat <- melt(dat, id.vars = c("year", "vessel_name"))
  p <- ggplot(dat) +
    aes(x = year, y = value, fill = vessel_name) +
    geom_col() +
    coord_cartesian(expand = FALSE) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Catch",translate=french, allow_missing = TRUE),"(t)"),
         fill = "") +
    scale_y_continuous(labels = scales::number(big.mark = ifelse(french, " ", ",")),
                       limits = c(0, 1.2*max(dat$value))) +
    scale_x_continuous(breaks = seq(0, last.yr, every)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  p
}



