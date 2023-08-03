b.rel.plot <- function(models,
  models.names,
  depl = FALSE,
  add.hist.ref = FALSE,
  add.bo.ref = FALSE,
  lrp = NA,
  usr = NA,
  proj_columns = NULL,
  tac_vector = 0,
  burnin = 1000,
  thin = 1,
  year_range = NULL,
  probs = c(0.025, 0.975),
  ylim = c(0, NA),
  col = c("red", "green"),
  x.every = 5, # Show x-axis label every n years
  french=FALSE
){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the biomass for the reference points

  pal <- unname(colorBlindness::availableColors()[-1])

  ## Biomass or Depletion
  if(depl){
    bt.quants <- lapply(models,
      function(x){
        j <- x$mcmccalcs$depl.quants
        rownames(j)[1] <- "lowercv"
        rownames(j)[2] <- "median"
        rownames(j)[3] <- "uppercv"
        j})
  }else{
    bt.quants <- lapply(models,
      function(x){
        j <- x$mcmccalcs$sbt.quants
        rownames(j)[1] <- "lowercv"
        rownames(j)[2] <- "median"
        rownames(j)[3] <- "uppercv"
        j})
  }

  if (!is.null(proj_columns)) {

    m <- models[[1]]
    stopifnot(all(tac_vector %in% unique(m$mcmc$proj$TAC)))
    proj_dat <- filter(m$mcmccalcs$proj.dat, TAC %in% tac_vector)
    proj_dat <- proj_dat[,c("TAC", proj_columns),drop=FALSE]
    names(proj_dat) <- gsub("^B", "", names(proj_dat))
    proj_dat <- reshape2::melt(proj_dat, id.vars = "TAC") %>%
      mutate(year = as.numeric(as.character(variable))) %>%
      select(-variable) %>%
      rename(B = value) %>%
      as_tibble() %>%
      group_by(TAC, year) %>%
      summarise(
        q05 = quantile(B, probs = probs[[1]]),
        q50 = quantile(B, probs = 0.50),
        q95 = quantile(B, probs = probs[[2]])
      )
  }

  names(bt.quants) <- models.names
  bt.quants <- lapply(bt.quants,
    function(x){
      tmp <- as.data.frame(t(x))
      tmp %>% mutate(Year = rownames(tmp))})
  bt <- bind_rows(bt.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
      models.names[1],
      after = 0)) %>%
    select(-MPD)

  ## B0 values
  r.quants <- lapply(models,
    function(x){
      x$mcmccalcs$r.quants})
  bo.raw <- lapply(r.quants,
    function(x){
      x[rownames(x) == "bo", ]})
  bo <- lapply(bo.raw,
    function(x){
      as.numeric(x[,2:4])})
  bo <- as.data.frame(do.call(rbind, bo))
  bo <- cbind(models.names, bo)

  names(bo) <- c("Sensitivity", "lowercv", "median", "uppercv")
  bo <- as.tibble(bo) %>%
    mutate(Year = min(bt$Year))

  horiz.offset <- 1.7
  p <- ggplot(bt, aes(x = Year,
    y = median,
    ymin = lowercv,
    ymax = uppercv))
  if (is.null(proj_columns)) {
    p <- p + geom_ribbon(alpha = 0.2,
      aes(fill = Sensitivity)) +
      geom_line(aes(color = Sensitivity), size = 1)+
      scale_color_manual(values=pal) +
      scale_fill_manual(values=pal)
    if(!depl){
      p <- p + geom_pointrange(data = bo,
        size = 0.25,
        position = position_dodge(width = horiz.offset),
        mapping = aes(color = Sensitivity),
        show.legend = FALSE)
    }
  } else {
    p <- p + geom_ribbon(alpha = 0.2, fill = "grey30") +
      geom_line(size = 1, colour = "grey30")
    if(!depl){
      p <- p + geom_pointrange(data = bo,
        size = 0.25,
        position = position_dodge(width = horiz.offset),
        show.legend = FALSE)
    }
  }

  p <- p + theme(legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.title = element_blank()) +
    # scale_y_continuous(labels = scales::number,
    #                    limits = ylim) +
    coord_cartesian(expand = FALSE) +
    xlim(c(min(bt$Year - 1), NA)) +
    scale_x_continuous(breaks = seq(0, 3000, x.every)) +
    ylab(paste(en2fr("Biomass",translate=french, allow_missing = TRUE),"(t)"))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))

  if(!depl & add.hist.ref){
    if(any(is.na(lrp)) || any(is.na(usr))){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      yrs <- bt$Year

      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2]) %>%
        mutate(lowercv = mean(lowercv),
          median = mean(median),
          uppercv = mean(uppercv))
      cal <- cal[1,]
      cal <- cal[rep(1, each = length(yrs)),]
      cal$Year <- yrs
      cal$Color <- col[[1]]
      p <- p + geom_ribbon(data = cal,
        alpha = 0.2,
        fill = cal$Color)
      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2]) %>%
        mutate(lowercv = mean(lowercv),
          median = mean(median),
          uppercv = mean(uppercv))
      cau <- cau[1,]
      cau <- cau[rep(1, each = length(yrs)),]
      cau$Year <- yrs
      cau$Color <- col[[2]]
      p <- p + geom_ribbon(data = cau,
        alpha = 0.2,
        fill = cau$Color)
      bt.slice <- bt
      if(!is.null(year_range)){
        bt.slice <- bt %>%
          filter(Year >= year_range[1] & Year <= year_range[2])
      }
      ylim <- c(ylim[1], max(cau$uppercv,
        bt.slice$uppercv,
        ylim[2]))

      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.val <- mean(cal$median)
      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2])
      usr.val <- mean(cau$median)
      j <- data.frame("Intercept" = c(lrp.val, usr.val),
        "Color" =  col)
      p <- p +
        geom_hline(data = j,
          aes(yintercept = Intercept),
          color = j$Color,
          linetype = "dashed",
          size = 1)
    }
  }

  if(add.bo.ref){
    bo.med <- as.numeric(bo[2])
    j <- data.frame("Intercept" = c(0.2*bo.med, 0.4*bo.med),
      "Color" = col)
    p <- p +
      geom_hline(data = j,
        aes(yintercept = Intercept),
        color = j$Color,
        linetype = "dashed",
        size = 1)
  }

  if(depl){
    p <- p + ylab(en2fr("Relative biomass",translate=french, allow_missing=TRUE))
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }

  if (!is.null(proj_columns)) {
    p <- p +
      geom_ribbon(data = proj_dat, aes(x = year, ymin = q05, ymax = q95,
        fill = as.factor(TAC)),
        inherit.aes = FALSE, alpha = 0.2) +
      geom_line(data = proj_dat, aes(x = year, y = q50, colour = as.factor(TAC)),
        inherit.aes = FALSE, lwd = 1, alpha = 1, lty = 1.2) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      theme(legend.position = c(1, 1), legend.title = element_text(size = 9, hjust = 0)) +
      labs(fill = "TAC (mt)", colour = "TAC (mt)") +
      geom_vline(xintercept = min(proj_dat$year), lty = 2, col = "grey80")
  }

  if (!is.null(year_range)) {
    p <- p + coord_cartesian(xlim = year_range)
  }
  #comma
  if(french){
    p <- p + scale_y_continuous(labels = scales::number,
      limits = ylim)
  } else{
    p <- p + scale_y_continuous(labels = comma,
      limits = ylim)
  }


  p
}


b.plot <- function(models,
                   models.names,
                   depl = FALSE,
                   add.hist.ref = FALSE,
                   add.bo.ref = FALSE,
                   lrp = NA,
                   usr = NA,
                   proj_columns = NULL,
                   tac_vector = 0,
                   burnin = 1000,
                   thin = 1,
                   year_range = NULL,
                   probs = c(0.025, 0.975),
                   ylim = c(0,NA),
                   col = c("red", "green"),
                   x.every = 5, # Show x-axis label every n years
                   french=FALSE
  ){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the biomass for the reference points

  pal <- unname(colorBlindness::availableColors()[-1])

  ## Biomass or Depletion
  if(depl){
    bt.quants <- lapply(models,
                        function(x){
                          j <- x$mcmccalcs$depl.quants
                          rownames(j)[1] <- "lowercv"
                          rownames(j)[2] <- "median"
                          rownames(j)[3] <- "uppercv"
                          j})
  }else{
    bt.quants <- lapply(models,
                        function(x){
                          j <- x$mcmccalcs$sbt.quants
                          rownames(j)[1] <- "lowercv"
                          rownames(j)[2] <- "median"
                          rownames(j)[3] <- "uppercv"
                          j})
  }

  if (!is.null(proj_columns)) {
    m <- models[[1]]
    stopifnot(all(tac_vector %in% unique(m$mcmc$proj$TAC)))
    proj_dat <- filter(m$mcmccalcs$proj.dat, TAC %in% tac_vector)
    proj_dat <- proj_dat[,c("TAC", proj_columns),drop=FALSE]
    names(proj_dat) <- gsub("^B", "", names(proj_dat))
    proj_dat <- reshape2::melt(proj_dat, id.vars = "TAC") %>%
      mutate(year = as.numeric(as.character(variable))) %>%
      select(-variable) %>%
      rename(B = value) %>%
      as_tibble() %>%
      group_by(TAC, year) %>%
      summarise(
        q05 = quantile(B, probs = probs[[1]]),
        q50 = quantile(B, probs = 0.50),
        q95 = quantile(B, probs = probs[[2]])
      )
  }

  names(bt.quants) <- models.names
  bt.quants <- lapply(bt.quants,
                      function(x){
                        tmp <- as.data.frame(t(x))
                        tmp %>% mutate(Year = rownames(tmp))})
  bt <- bind_rows(bt.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  ## B0 values
  r.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$r.quants})
  bo.raw <- lapply(r.quants,
                   function(x){
                     x[rownames(x) == "bo", ]})
  bo <- lapply(bo.raw,
               function(x){
                 as.numeric(x[,2:4])})
  bo <- as.data.frame(do.call(rbind, bo))
  bo <- cbind(models.names, bo)

  names(bo) <- c("Sensitivity", "lowercv", "median", "uppercv")
  bo <- as.tibble(bo) %>%
    mutate(Year = min(bt$Year))

  horiz.offset <- 1.7
  p <- ggplot(bt, aes(x = Year,
                      y = median,
                      ymin = lowercv,
                      ymax = uppercv))
  if (is.null(proj_columns)) {
    p <- p + geom_ribbon(alpha = 0.2,
                         aes(fill = Sensitivity)) +
      geom_line(aes(color = Sensitivity), size = 1)+
      scale_color_manual(values=pal) +
      scale_fill_manual(values=pal)
    if(!depl){
      p <- p + geom_pointrange(data = bo,
        size = 0.25,
        position = position_dodge(width = horiz.offset),
        mapping = aes(color = Sensitivity),
        show.legend = FALSE)
    }
  } else {
    p <- p + geom_ribbon(alpha = 0.2, fill = "grey30") +
      geom_line(size = 1, colour = "grey30")
    if(!depl){
      p <- p + geom_pointrange(data = bo,
        size = 0.25,
        position = position_dodge(width = horiz.offset),
        show.legend = FALSE)
    }
  }

  p <- p + theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
   # scale_y_continuous(labels = scales::number,
   #                    limits = ylim) +
    coord_cartesian(expand = FALSE) +
    xlim(c(min(bt$Year - 1), NA)) +
    scale_x_continuous(breaks = seq(0, 3000, x.every)) +
    ylab(paste(en2fr("Biomass",translate=french, allow_missing = TRUE),"(t)"))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))

  if(!depl & add.hist.ref){
    if(any(is.na(lrp)) || any(is.na(usr))){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      yrs <- bt$Year

      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2]) %>%
        mutate(lowercv = mean(lowercv),
               median = mean(median),
               uppercv = mean(uppercv))
      cal <- cal[1,]
      cal <- cal[rep(1, each = length(yrs)),]
      cal$Year <- yrs
      cal$Color <- col[[1]]
      p <- p + geom_ribbon(data = cal,
                           alpha = 0.2,
                           fill = cal$Color)
      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2]) %>%
        mutate(lowercv = mean(lowercv),
               median = mean(median),
               uppercv = mean(uppercv))
      cau <- cau[1,]
      cau <- cau[rep(1, each = length(yrs)),]
      cau$Year <- yrs
      cau$Color <- col[[2]]
      p <- p + geom_ribbon(data = cau,
                           alpha = 0.2,
                           fill = cau$Color)
      bt.slice <- bt
      if(!is.null(year_range)){
        bt.slice <- bt %>%
          filter(Year >= year_range[1] & Year <= year_range[2])
      }
      ylim <- c(ylim[1], max(cau$uppercv,
                             bt.slice$uppercv,
                             ylim[2]))

      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.val <- mean(cal$median)
      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2])
      usr.val <- mean(cau$median)
      j <- data.frame("Intercept" = c(lrp.val, usr.val),
                      "Color" =  col)
      p <- p +
        geom_hline(data = j,
                   aes(yintercept = Intercept),
                   color = j$Color,
                   linetype = "dashed",
                   size = 1)
    }
  }

  if(add.bo.ref){
    bo.med <- as.numeric(bo[2])
    j <- data.frame("Intercept" = c(0.2*bo.med, 0.4*bo.med),
                    "Color" = col)
    p <- p +
        geom_hline(data = j,
                   aes(yintercept = Intercept),
                   color = j$Color,
                   linetype = "dashed",
                   size = 1)
  }

  if(depl){
    p <- p + ylab(en2fr("Relative biomass",translate=french, allow_missing=TRUE))
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }

  if (!is.null(proj_columns)) {
    p <- p +
      geom_ribbon(data = proj_dat, aes(x = year, ymin = q05, ymax = q95,
        fill = as.factor(TAC)),
        inherit.aes = FALSE, alpha = 0.2) +
      geom_line(data = proj_dat, aes(x = year, y = q50, colour = as.factor(TAC)),
        inherit.aes = FALSE, lwd = 1, alpha = 1, lty = 1.2) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      theme(legend.position = c(1, 1), legend.title = element_text(size = 9, hjust = 0)) +
      labs(fill = "TAC (mt)", colour = "TAC (mt)") +
      geom_vline(xintercept = min(proj_dat$year), lty = 2, col = "grey80")
  }

  if (!is.null(year_range)) {
    p <- p + coord_cartesian(xlim = year_range)
    }
 #comma
  if(french){
    p <- p + scale_y_continuous(labels = scales::number,
                              limits = ylim)
  } else{
    p <- p + scale_y_continuous(labels = comma,
                                limits = ylim)
  }


  p
}

b.plot.mpd <- function(models,
                       models.names,
                       depl = FALSE,
                       french=FALSE){
  yrs <- lapply(models,
                function(x){
                  x$mpd$syr:(x$mpd$nyr + 1)})

  if(depl){
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt / x$mpd$sbo })
  }else{
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt})
  }
  bt <- lapply(1:length(bt),
               function(x){
                 tmp <- as.data.frame(t(bt[[x]]))
                 rownames(tmp) <- "Biomass (t)"
                 colnames(tmp) <-  yrs[[x]]
                 tmp})
  names(bt) <- models.names

  bt <- bind_rows(bt, .id = "Sensitivity") %>%
    melt() %>%
    as.tibble() %>%
    mutate(Year = variable, `Biomass (t)` = value) %>%
    select(-c(variable, value)) %>%
    mutate(Year = as.numeric(as.character(Year))) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0))

  bo <- lapply(models,
               function(x){
                 x$mpd$sbo
               })
  names(bo) <- models.names
  bo <- t(bind_cols(bo))
  bo <- cbind(rownames(bo), bo, min(bt$Year))
  colnames(bo) <- c("Sensitivity", "Biomass (t)", "Year")
  bo <- bo %>%
    as.tibble() %>%
    mutate(`Biomass (t)` = as.numeric(`Biomass (t)`),
           Year = as.numeric(Year))

  horiz.offset <- 1.7
  p <- ggplot(bt, aes(x = Year,
                      y = `Biomass (t)`,
                      #ymin = 0,
                      #ymax = max(`Biomass (t)`),
                      group = Sensitivity)) +
    geom_line(aes(color = Sensitivity),
              size = 1) +
        theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 5))+
    ylab(paste(en2fr("Biomass",translate=french, allow_missing = TRUE),"(t)"))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))


  if(!depl){
    p <- p + geom_point(data = bo,
                        size = 2,
                        position = position_dodge(width = horiz.offset),
                        mapping = aes(color = Sensitivity),
                        show.legend = FALSE)
  }

  if(depl){
    p <- p + ylab(en2fr("Relative biomass",translate=french, allow_missing=TRUE))
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }

  p
}
