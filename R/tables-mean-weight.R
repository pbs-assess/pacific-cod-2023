# get a list of the mean weights and add NAs for missing years
# Arguments like this:
# models <- c(base.model.3cd, sens.models.11)
# model.names <- c(base.model.3cd.name, sens.models.name.11)

# Robyn Forrest, June 6 2022

mw.table <- function(models,
                     model.names,
                     years=2010:2020,
                     area="3CD",
                     caption = caption){

    years <- years
    # get all the mean weights in a list format
    mean.wts.list <- lapply(models,
           function(x){
              mpd <- x$mpd
              yrs <- x$dat$meanwtdata[,1]
              obs <- mpd$obs_annual_mean_weight
              tab <- cbind(yrs, obs) %>% as_tibble() %>%
                filter(yrs %in% years)
              # now add missing years
              missing <- years[!years %in% tab$yrs]
              if(length(missing)>0){
                tmp <- cbind(missing,NA) %>%
                  as.data.frame() %>%
                  rename(yrs = names(.)[1], obs = names(.)[2]) %>%
                  rbind(tab) %>%
                  arrange(yrs)
                tab <- tmp}

              tab <- tab$obs
              tab
              })


    # bind together in a table and add row names
    mean.wts <- do.call(cbind, lapply(mean.wts.list, as.data.frame))
    mean.wts <- cbind(years,mean.wts)
    colnames(mean.wts) <- c("Year",model.names)

    filename <- here::here("report", paste0("MeanWeightTable_",area ,".csv"))
    write_csv(mean.wts, filename)

    knitr::kable(mean.wts,
                  caption = caption,
                  longtable = TRUE, format = "pandoc",
                  align = get.align(ncol(mean.wts))[-1],
                  booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE) %>%
     kableExtra::kable_styling(latex_options = c("hold_position", "repeat_header"))

 }# end function

