i.plot <- function(models,
                   models.names,
                   ind,
                   every = 1,
                   leg.loc = "topright",
                   show.fit = TRUE,
                   french=FALSE){
  ## ind is the index number
  ## every is show every nth year on the x-axis
  ## leg.loc: topright, topleft, bottomright, bottomleft

  index.fit <- lapply(models,
                      function(x){
                        tmp <- x$mpd$it_hat[ind,]
                        tmp[!is.na(tmp)]
                      })
  index.dat <- lapply(models,
                      function(x){
                        as.data.frame(x$dat$indices[[ind]])
                      })

  i <- lapply(1:length(models),
              function(x){
                cbind(index.dat[[x]], fit = index.fit[[x]])
              })
  names(i) <- models.names
  i <- bind_rows(i, .id = "Sensitivity") %>%
    as.tibble() %>%
    mutate(lowercv = exp(log(it) + qnorm(0.025) * (1 / wt)),
           uppercv = exp(log(it) + qnorm(0.975) * (1 / wt))) %>%
    rename("Year" = iyr,
           "Survey biomass index (t)" = it) %>%
    select(-c(gear, area, group, sex, wt, timing)) %>%
    mutate(Year = as.integer(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0))

  if(leg.loc == "topright"){
    leg.just <- c(1, 1)
    leg.pos <- c(1, 1)
  }else if(leg.loc == "topleft"){
    leg.just <- c(0, 1)
    leg.pos <- c(0, 1)
  }else if(leg.loc == "bottomright"){
    leg.just <- c(1, 0)
    leg.pos <- c(1, 0)
  }else if(leg.loc == "bottomleft"){
    leg.just <- c(0, 0)
    leg.pos <- c(0, 0)
  }

  p <- ggplot(i) +
    aes(x = Year, y = `Survey biomass index (t)`) +
    geom_pointrange(aes(ymin = lowercv,
                        ymax = uppercv),
                    size = 0.25) +
    theme(legend.position = leg.pos,
          legend.justification = leg.just,
          legend.title = element_blank()) +
    coord_cartesian(expand = FALSE) +
    labs(x = en2fr("Year",translate=french, allow_missing = TRUE),
         y = paste(en2fr("Survey biomass index",translate=french, allow_missing = TRUE),"(t)"),
         fill = "") +
    scale_x_continuous(breaks = seq(0, 3000, every),
                       limits = c(min(i$Year - 1),
                                  max(i$Year + 1))) #+
  if(show.fit){
    p <- p + geom_line(data = i,
                       aes(color = Sensitivity),
                       y = i$fit,
                       size = 1)
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
