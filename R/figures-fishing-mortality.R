scaleFUN <- function(x) sprintf("%.2f", x)

f.plot <- function(models,
                   models.names,
                   add.hist.ref = FALSE,
                   lrr = NA,
                   french=FALSE){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the F value for the reference points

  f.quants <- lapply(models,
                     function(x){
                       ## Assume only first gear has F's
                       j <- x$mcmccalcs$f.mort.quants[[1]]
                       rownames(j)[1] <- "lowercv"
                       rownames(j)[2] <- "median"
                       rownames(j)[3] <- "uppercv"
                       j})

  names(f.quants) <- models.names
  f.quants <- lapply(f.quants,
                      function(x){
                        tmp <- as.data.frame(t(x))
                        tmp %>% mutate(Year = rownames(tmp))})
  f <- bind_rows(f.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Fishing mortality` = median) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  p <- ggplot(f, aes(x = Year,
                      y = `Fishing mortality`,
                      ymin = lowercv,
                      ymax = uppercv,
                      fill = Sensitivity)) +
    geom_ribbon(alpha = 0.2) +
    geom_line(aes(color = Sensitivity),
              size = 1) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    #scale_y_continuous(labels = scaleFUN,
    #                   limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    xlim(c(min(f$Year - 1), NA)) +
    scale_x_continuous(breaks = seq(0, 3000, 5))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))+
    ylab(en2fr("Fishing mortality",translate=french, allow_missing = TRUE))

  if(add.hist.ref){
    if(is.na(lrr[1])){
      cat0("Supply year ranges for lrr when add.hist.ref is TRUE")
    }else{
       yrs <- f$Year

       cal <- f %>%
       filter(Year >= lrr[1] & Year <= lrr[2])

       lrr.val <- mean(cal$`Fishing mortality`)

       cal <- f %>%
         filter(Year >= lrr[1] & Year <= lrr[2]) %>%
         mutate(lowercv = mean(lowercv),
                `Fishing mortality` = mean(`Fishing mortality`),
                uppercv = mean(uppercv))

       cal <- cal[1,]
       cal <- cal[rep(1, each = length(yrs)),]
       cal$Year <- yrs
       cal$Color <- 1
       p <- p + geom_ribbon(data = cal,
                            alpha = 0.2,
                          fill = cal$Color)

       j <- data.frame("Intercept" = c(lrr.val),
                       "Color" = c("black"))
       p <- p +
         geom_hline(data = j,
                    aes(yintercept = Intercept),
                   color = j$Color,
                    linetype = "dashed",
                    size = 1,
                    show.legend = TRUE)
    }
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
