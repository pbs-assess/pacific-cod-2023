r.plot <- function(models,
                   models.names,
                   add.meds = FALSE,
                   x.axis.angle = 0,
                   french=FALSE){
  ## add.meds: Add median and mean line
  ## x.axis.angle: Angle of x-axis labels

  rt <- lapply(models,
               function(x){
                 j <- x$mcmccalcs$recr.quants
                 rownames(j)[1] <- "lowercv"
                 rownames(j)[2] <- "median"
                 rownames(j)[3] <- "uppercv"
                 j})

  yrs <- lapply(rt,
                function(x){
                  as.numeric(colnames(x))})

  names(rt) <- models.names
  rt <- lapply(rt,
               function(x){
                 tmp <- as.data.frame(t(x))
                 tmp %>% mutate(Year = rownames(tmp))})
  rt <- bind_rows(rt, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Recruits (thousands)` = median) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  rt.median <- median(rt$`Recruits (thousands)`)
  rt.mean <- mean(rt$`Recruits (thousands)`)

  horiz.offset <- 2
  p <- ggplot(rt, aes(x = Year,
                      y = `Recruits (thousands)`,
                      ymin = lowercv,
                      ymax = uppercv)) +
    geom_pointrange(data = rt,
                    size = 0.25,
                    position = position_dodge(width = horiz.offset),
                    mapping = aes(color = Sensitivity)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = x.axis.angle)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 5)) +
    ylab(paste(en2fr("Recruitment",translate=french, allow_missing = TRUE),paste0("(",en2fr("thousands",translate=french, allow_missing = TRUE, case = "lower"),")")))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))

  if(add.meds){
    j <- data.frame("Intercept" = c(rt.median, rt.mean),
                    "Color" = c("blue", "green"))
    p <- p + geom_hline(data = j,
                        aes(yintercept = Intercept),
                        color = j$Color,
                        linetype = "dashed",
                        size = 1,
                        show.guide = FALSE)
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}

r.devs.plot <- function(models,
                        models.names,
                        x.axis.angle = 0,
                        french=FALSE){

  rt <- lapply(models,
               function(x){
                 j <- x$mcmccalcs$recr.devs.quants
                 rownames(j)[1] <- "lowercv"
                 rownames(j)[2] <- "median"
                 rownames(j)[3] <- "uppercv"
                 j})

  yrs <- lapply(rt,
                function(x){
                  as.numeric(colnames(x))})

  names(rt) <- models.names
  rt <- lapply(rt,
               function(x){
                 tmp <- as.data.frame(t(x))
                 tmp %>% mutate(Year = rownames(tmp))})
  rt <- bind_rows(rt, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Log recruitment deviations` = median) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0))

  rt.median <- median(rt$`Log recruitment deviations`)
  rt.mean <- mean(rt$`Log recruitment deviations`)

  horiz.offset <- 2
  p <- ggplot(rt, aes(x = Year,
                      y = `Log recruitment deviations`,
                      ymin = lowercv,
                      ymax = uppercv)) +
    geom_pointrange(data = rt,
                    size = 0.25,
                    position = position_dodge(width = horiz.offset),
                    mapping = aes(color = Sensitivity)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = x.axis.angle)) +
    scale_y_continuous(labels = comma) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 5)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               size = 1)+
    ylab(en2fr("Log recruitment deviations",translate=french, allow_missing = TRUE))+
    xlab(en2fr("Year",translate=french, allow_missing = TRUE))

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
