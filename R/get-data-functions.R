## Functions to extract PCod data and get catch, survey indices, and mean weights:
## For indices:
## 5CD - use HSMAS, HSSS, and CPUE
## 5AB - use QCSSS and CPUE
## 3CD - use WCVISS and CPUE

#' Load the data from the RDS files produced by extract.data()
#'
#' @param cache.dir Relative name of the directory to hold the RDS files
#'
#' @return The data object as returned from gfplot package
load.data <- function(cache.dir = file.path(rootd.data, "cache")){
  readRDS(file.path(cache.dir, "pacific-cod.rds"))
}

#' Extract table of commercial specimens for areas requested
#'
#' @param dat A tibble of the commercial samples from gfdata package
#' @param a Growth parameter alpha
#' @param b Growth parameter beta
#'
#' @return A tibble of all commercial specimens,
#'   with new columns 'month', 'quarter', and 'calc.weight' added
#'   calc.weight is calculated from length according to the Appendix in the
#'   2018 PCod stock assessment.
comm.specimens  <- function(dat,
                            a,
                            b){

  dat <- mutate(dat,
                month = month(trip_start_date),
                quarter = case_when(
                  month %in% seq(1, 3) ~ 4,
                  month %in% seq(4, 6) ~ 1,
                  month %in% seq(7, 9) ~ 2,
                  month %in% seq(10, 12) ~ 3
                )) %>% select(-month) %>%
    mutate(calc.weight = a * length ^ b) %>%
    filter(year >= 1956)

  ## Filter the data as shown in the table in the appendix for the calculation
  ##  of mean weight data (Table C1 in 2013 assessment document)
  ## Removed filtration of KEEPERS and UNSORTED so that data match 2014 data.
  dat %>% filter(trip_sub_type_code %in% c(1, 4)) %>%
    filter(gear_code == 1) %>%
    #filter(gear == 1) %>%
    ## filter(year <= 1996 & sampling_desc == "KEEPERS" |
    ##       year > 1996 & sampling_desc == "UNSORTED") %>%
    filter(sample_type_code %in% c(1, 2, 6, 7)) %>%
    filter(!sample_id %in% c(173726,
                             173740,
                             191471,
                             184243,
                             184159,
                             215903,
                             223726))}

#' Calculate the mean weight by sample
#'
#' @param dat A tibble of the catch from gfplot package
#'
#' @return A tibble of mean weight by sample
calc.mean.weight <- function(dat){
  ## Eq C3 in 2013 PCod assessment
  dat <- dat %>%
    group_by(year, quarter, sample_id) %>%
    mutate(calc.sample.weight = sum(calc.weight, na.rm = TRUE),
           sample_weight = ifelse(!is.na(sample_weight),
                                  sample_weight,
                                  calc.sample.weight)) %>%
    summarize(numerator = sum(mean(calc.weight, na.rm = TRUE) * sample_weight[1]),
              denominator = sample_weight[1],
              catch_weight = total_catch[1]) %>%
    summarize(ws = sum(numerator) / sum(denominator),
              catch_weight = catch_weight[1])
  ## Eq C4 in 2013 PCod assessment
  dat %>% group_by(year, quarter) %>%
    summarize(numerator = sum(ws * catch_weight),
              denominator = sum(catch_weight),
              wf = numerator / denominator) %>%
    ungroup()
}

#' Calculate the total catch by year and quarter
#'
#' @param dat A tibble of the catch from gfplot package
#' @param area A vector of regexps like this: c("3[CD]+", "5[CD]+")
#'   Used only for USA data.
#' @param include.usa Toggle to include the USA catch in the table and total_catch
#'  column sums
#'
#' @return A tibble with the year and sum of landed and discarded catch
#'   by year and quarter.
total.catch.yr.qtr <- function(dat,
                               areas = NULL,
                               include.usa = FALSE){
  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  dat <- mutate(dat,
                area = assign_areas(major_stat_area_name,
                                    area_regex = areas)) %>%
    filter(!is.na(area))

  ## Bring in USA landings from csv files
  areas <- gsub("\\[|\\]|\\+", "", areas)
  area.num <- substr(areas[1],
                     grep("[0-9]", areas),
                     grep("[0-9]", areas))

  usa <- tibble(year = 0L, usa_catch = 0L)
  if(length(grep("AB", areas))){
    fn <- paste0(file.path(rootd.data, "usa-catch-"), area.num, "AB.csv")
    usa <- as_tibble(read.csv(fn))
  }
  if(length(grep("CD", areas))){
    fn <- paste0(file.path(rootd.data, "usa-catch-"), area.num, "CD.csv")
    usa.cd <- as_tibble(read.csv(fn))
    if(nrow(usa) > 1){
      ## Merge the two area catches (sum) into a single tibble
      usa <- left_join(usa, usa.cd, by = "year") %>%
        mutate(usa_catch.x, usa_catch.x = if_else(is.na(usa_catch.x),
                                                  0L,
                                                  usa_catch.x)) %>%
        mutate(usa_catch.y, usa_catch.y = if_else(is.na(usa_catch.y),
                                                  0L,
                                                  usa_catch.y)) %>%
        mutate(usa_catch = usa_catch.x + usa_catch.y) %>%
        select(-c("usa_catch.x", "usa_catch.y"))
    }else{
      usa <- usa.cd
    }
  }

  if(!include.usa){
    ## Set all USA catch to 0
    usa <- mutate(usa, usa_catch = 0L)
  }
  mutate(dat,
         month = month(best_date),
         quarter = case_when(
           month %in% c(1, 2, 3) ~ 4,
           month %in% c(4, 5, 6) ~ 1,
           month %in% c(7, 8, 9) ~ 2,
           month %in% c(10, 11, 12) ~ 3
         )) %>%
    select(-month) %>%
    mutate(year = if_else(quarter == 4, year - 1, as.numeric(year))) %>%
    group_by(year, quarter) %>%
    summarize(
      discarded_canada = sum(discarded_kg) / 1000,
      landed_canada = sum(landed_kg) / 1000,
      catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    left_join(usa, by = "year") %>%
    rowwise() %>%
    ## Spread the USA catch out over 4 quarters as it is year-based
    mutate(usa_catch, usa_catch1 = usa_catch / 4) %>%
    ## mutate(usa_catch, usa_catch1 = if_else(quarter == 1,
    ##                                        usa_catch,
    ##                                        0L)) %>%
    mutate(usa_catch1, usa_catch2 = if_else(is.na(usa_catch1),
                                            0,
                                            usa_catch1)) %>%
    mutate(total_catch = sum(usa_catch2, catch_weight)) %>%
    mutate(total_catch = round(total_catch, 3)) %>%
    ## mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
    ungroup() %>%
    select(-c(usa_catch, usa_catch1)) %>%
    rename(canada_catch = catch_weight,
           usa_catch = usa_catch2)
}

total.catch.discards <- function(dat,
                                 areas = NULL){

  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  dat <- mutate(dat,
                area = assign_areas(major_stat_area_name,
                                    area_regex = areas)) %>%
    filter(!is.na(area))
  dat %>%
    select(year, landed_kg, discarded_kg)
}

#' Extract survey biomass and CV for the requested survey
#'
#' @param dat A tibble of the survey index from gfplot package
#' @param survey.series.id 1 = QCSSS, 2 = HSMAS, 3 = HSSS, 4 = WCVISS
#'
#' @return A tibble of the survey biomass index and wt for the requested survey
get.survey.index <- function(dat,
                             survey.series.id){
  dat %>%
    filter(survey_series_id == survey.series.id) %>%
    mutate(wt = 1/re) %>%
    select(year, biomass, wt) %>%
    mutate(biomass = sprintf("%0.1f", biomass / 1000),
           wt = sprintf("%0.1f", wt))
}

#' Calculate the mean weight by year
#'
#' @param dat.catch A tibble of the catch from gfplot package
#' @param dat.comm.samples A tibble of the commercial samples from gfplot package
#' @param areas A regexp like this: "5[CD]+"
#' @param a Growth parameter alpha
#' @param b Growth parameter beta
#'
#' @return A tibble of year and mean weight
get.mean.weight <- function(dat.comm.samples,
                            dat.catch,
                            areas =  c("3[CD]+", "5[AB]+", "5[CD]+"),
                            include.usa = FALSE,
                            a,
                            b){

  dat.comm.samples <- mutate(dat.comm.samples,
                             area = assign_areas(major_stat_area_name,
                                                 area_regex = areas)) %>%
    filter(!is.na(area))

  cs <- comm.specimens(dat.comm.samples, a, b)
  cs$catch_weight <- NULL
  tot.catch <- total.catch.yr.qtr(dat.catch,
                                  areas = areas,
                                  include.usa = include.usa)

  samp_n <- cs |> group_by(year) |>
    summarize(
      n_samples = length(unique(sample_id)),
      n_specimens = length(unique(specimen_id))
    )

  out <- left_join(cs, tot.catch, by = c("year", "quarter")) %>%
    calc.mean.weight() %>%
    group_by(year) %>%
    summarize(mean_weight = mean(wf)) %>%
    filter(!is.na(mean_weight)) %>%
    mutate(mean_weight = as.numeric(sprintf("%0.3f", mean_weight)))

  left_join(out, samp_n)
}


proc.old <- function(area = "3[CD]+"){
  d <- read.csv(file.path(rootd.data, "Pcod_Catch_all_by_major_area_FY_Q.csv)"))
  d <- d %>%
    mutate(area = assign_areas(Area, area)) %>%
    filter(!is.na(area)) %>%
    group_by(FYear, Quarter) %>%
    mutate(catch = (Landed_kg + Released_kg) / 1000.0)

  area <- gsub("\\^|\\[|\\]|\\+", "", area)
  write.csv(d,
            file = paste0(file.path(rootd.data, "results/Old-catch-by-year-quarter-"), area, ".csv"),
            row.names = FALSE)

  d <- group_by(d, FYear) %>% summarize(year_catch = sum(catch))
  write.csv(d,
            file = paste0(file.path(rootd.data, "results/Old-catch-by-year-"), area, ".csv"),
            row.names = FALSE)
}

proc.old.catch <- function(area = "3[CD]+",
                           include.usa = TRUE){
  dat <- load.data(cache.dir = file.path(rootd.data, "pcod-cache"))
  d <- dat$commercial_samples

  catch <- read.csv(file.path(rootd.data, "Pcod_Catch_all_by_major_area_FY_Q.csv"))
  catch <- catch %>%
    group_by(FYear, Quarter) %>%
    mutate(total_catch = (Landed_kg + Released_kg) / 1000.0) %>%
    mutate(quarter = if_else(Quarter == "Jan-Mar",
                             4,
                             if_else(Quarter == "Apr-Jun",
                                     1,
                                     if_else(Quarter == "Jul-Sep",
                                             2,
                                             3)))) %>%
    mutate(year = FYear,
           landed_kg = Landed_kg,
           discarded_kg = Released_kg) %>%
    mutate(major_stat_area_name = Area) %>%
    mutate(best_date = if_else(quarter == 1,
                               4,
                               if_else(quarter == 2,
                                       7,
                                       if_else(quarter == 3,
                                               10,
                                               1))))

  df <- get.mean.weight(d,
                        catch,
                        areas = area,
                        include.usa = include.usa,
                        a = .ALPHA5,
                        b = .BETA5)

  df2 <- get.mean.weight(d,
                         dat$catch,
                         areas = area,
                         include.usa = include.usa,
                         a = .ALPHA5,
                         b = .BETA5)

  area <- gsub("\\^|\\[|\\]|\\+", "", area)

  analysis <- "2014 catch calc"
  df <- cbind(df, rep(analysis, nrow(df)))
  colnames(df) <- c("year","mean_weight","analysis")
  analysis <- "2018 catch calc"
  df2 <- cbind(df2, rep(analysis, nrow(df2)))
  colnames(df2) <- c("year","mean_weight","analysis")

  dfcompare <- rbind(df, df2)

  ggplot(data = dfcompare,
         aes(x = year,
             y = mean_weight,
             group = analysis,
             color = analysis)) +
    geom_line(lwd = 1) +
    ylim(0, 1.1 * max(dfcompare$mean_weight)) +
    theme(plot.title = element_text(size = 14,
                                    face = "bold",
                                    hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14,
                                    face = "bold")) +
    scale_x_continuous(breaks = seq(min(dfcompare$year),
                                    max(dfcompare$year),
                                    by = 5)) +
    labs(x = "Fishing Year",
         y = "Annual Mean Weight (Kg)",
         title = paste0("Area ", area))

  ggsave(file.path(rootd.data, "results", paste0("AnnualMeanWeight_", area, "_old_catch.png")),
         width = 8,
         height = 6,
         units = "in")

}

compare.catch <- function(area = "3[CD]+",
                          include.usa = TRUE){
  d.old <- read.csv(file.path(rootd.data, "Pcod_Catch_all_by_major_area_FY_Q.csv"))
  d.old <- d.old %>%
    mutate(area = assign_areas(Area, area)) %>%
    filter(!is.na(area)) %>%
    group_by(FYear, Quarter) %>%
    mutate(catch = (Landed_kg + Released_kg) / 1000.0) %>%
    group_by(FYear) %>%
    summarize(year_catch = sum(catch)) %>%
    mutate(year = FYear) %>%
    select(-FYear)

  dat <- load.data(cache.dir = file.path(rootd.data, "pcod-cache"))
  dat.catch <- mutate(dat$catch,
                      area = assign_areas(major_stat_area_name,
                                          area_regex = area)) %>%
    filter(!is.na(area))

  d <- total.catch.yr.qtr(dat.catch,
                          areas = area,
                          include.usa = include.usa) %>%
    group_by(year) %>%
    summarize(year_catch = sum(total_catch))

  x <- left_join(d.old, d, by = "year") %>%
    mutate(catch.old = year_catch.x,
           catch.new = year_catch.y) %>%
    select(-c(year_catch.x, year_catch.y)) %>%
    mutate(difference = catch.old - catch.new) %>%
    filter(!is.na(difference))

  area <- gsub("\\^|\\[|\\]|\\+", "", area)

  d <- melt(x, id.vars = "year") %>%
    mutate(variable = if_else(variable == "catch.old",
                              "2014 calculation",
                              if_else(variable == "catch.new",
                                      "2018 calculation",
                                      "Difference")))


  ggplot(d) +
    geom_line(aes(x=year, y=value, color=variable)) +
    theme(plot.title = element_text(size = 14,
                                  face = "bold",
                                  hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14,
                                    face = "bold")) +
    scale_x_continuous(breaks = seq(min(x$year),
                                    max(x$year),
                                    by = 5)) +
    labs(x = "Fishing Year", y = "Catch (t)",
         title = paste0("Area ", area))

  ggsave(file.path(rootd.data, "results", paste0("Catch-", area, ".png")),
         width = 8,
         height = 6,
         units = "in")
}

plot.spec <- function(area = "5[CD]+"){

  a <- gsub("\\^|\\[|\\]|\\+", "", area)
  if(a == "5CD"){
    d <- read.csv(file.path(rootd.data, "Pcod_commercial_specimens_5CD_June2014.csv"))
  }else if(a == "5AB"){
    d <- read.csv(file.path(rootd.data, "Pcod_commercial_specimens_5AB_June2014.csv"))
  }else{
    stop("Only implemented for 5AB and 5CD.")
  }

  d2014 <- as_tibble(d) %>%
    mutate(area = assign_areas(Area, area)) %>%
    filter(!is.na(area)) %>%
    mutate(quarter = if_else(Quarter == "Jan-Mar",
                             4,
                             if_else(Quarter == "Apr-Jun",
                                     1,
                                     if_else(Quarter == "Jul-Sep",
                                             2,
                                             3)))) %>%
    group_by(F.Year, quarter) %>%
    summarize(num.specimens = length(SPECIMEN_ID))

  colnames(d2014) <- c("year", "quarter", "num.specimens")

  d <- load.data(cache.dir = file.path(rootd.data, "pcod-cache"))
  d2018 <- mutate(d$commercial_samples,
                      area = assign_areas(major_stat_area_name,
                                          area_regex = area)) %>%
    filter(!is.na(area)) %>%
  mutate(month = month(trip_start_date),
           quarter = case_when(
             month %in% c(1, 2, 3) ~ 4,
             month %in% c(4, 5, 6) ~ 1,
             month %in% c(7, 8, 9) ~ 2,
             month %in% c(10, 11, 12) ~ 3
           )) %>%
    select(-month) %>%
  group_by(year, quarter) %>%
  summarize(num.specimens = length(specimen_id))

  j <- left_join(d2018, d2014, by = c("year", "quarter")) %>%
    subset(year >= 1956) %>%
    mutate(diff = num.specimens.x - num.specimens.y)
  colnames(j) <- c("year",
                   "quarter",
                   "num.specimens.2018",
                   "num.specimens.2014",
                   "num.specimens.2018 - 2014")
  k <- melt(j,
            id.vars = c("year", "quarter")) %>%
    mutate(labels = paste0(year, "-", quarter))

  area <- gsub("\\^|\\[|\\]|\\+", "", area)
  brk <- seq(1, length(k$labels), by = 1)

  ggplot(k) +
    geom_line(aes(x=labels,
                  y=value,
                  group = variable,
                  color = variable)) +
        theme(plot.title = element_text(size = 14,
                                  face = "bold",
                                  hjust = 0.5),
              axis.text.x = element_text(size = 4,
                                         angle = 90,
                                         hjust = .01),
              axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14,
                                    face = "bold")) +
    scale_x_discrete(breaks = k$labels[brk],
                     labels = k$labels[brk]) +
    labs(x = "Year and quarter", y = "Number of length specimens",
         title = paste0("Area ", area))

  ggsave(file.path(rootd.data, "results", paste0("Num-length-specimens-", area, ".png")),
         width = 12,
         height = 6,
         units = "in")

}

extrap.catch <- function(dat = load.data(cache.dir = file.path(rootd.data, "pcod-cache")),
                         areas = NULL,
                         nyr,
                         mnth){
  ## Extrapolate catch data for missing quarters based on the
  ## mean of the catch in the quarters in the previous nyr years.
  ## mnth is the last full month of catch data where 1=Apr..12=March
  ## Call like this:
  ## tmp = extrap.catch(areas="5[ABCD]+", nyr=5, mnth=6)

  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  dat <- mutate(dat$catch,
                area = assign_areas(major_stat_area_name,
                                    area_regex = areas)) %>%
    filter(!is.na(area))

  last.yr <- max(unique(dat$year))
  dat.prev <- mutate(dat,
                month = month(best_date),
                day = day(best_date)) %>%
    mutate(year = if_else(month <= 3, year - 1, as.numeric(year))) %>%
    mutate(month = if_else(month >= 4, month - 3, month + 9)) %>%
    filter(year >= (last.yr - nyr) &
           year < last.yr) %>%
    group_by(year, month) %>%
    summarize(catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    mutate(ccatch = cumsum(catch_weight)) %>%
    mutate(pcatch = ccatch / sum(catch_weight))

  pmeans <- sapply(lapply(1:12,
                          function(x){
                            tmp <- dat.prev %>% filter(month == x)
                            mean(tmp$pcatch)
                          }),
                   "[")

  ## Tabulate dat.prev
  dat.orig <- dat.prev
  dat.prev <- dat.prev %>%
    select(-c(catch_weight, ccatch))

  dat.props <- spread(dat.prev, month, pcatch)
  mn <- substring(month.name, 1, 3)
  names(dat.props) <- c("Year", mn[4:12], mn[1:3])
  dat.props$Year <- paste0(dat.props$Year - 2000, "/", dat.props$Year - 1999)

  dat.last <- mutate(dat,
                   month = month(best_date),
                   day = day(best_date)) %>%
    mutate(year = if_else(month <= 3, year - 1, as.numeric(year))) %>%
    mutate(month = if_else(month >= 4, month - 3, month + 9)) %>%
    filter(year == last.yr) %>%
    group_by(year, month) %>%
    summarize(catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    mutate(ccatch = cumsum(catch_weight))

  ## The last value in the list is the amount of catch that should be addedd to the final
  ## year's catch to allow for extrapolation for the remainder of the year after mnths months.
  list(dat.orig,
       dat.last,
       dat.props,
       dat.last[dat.last$month == mnth,]$ccatch / pmeans[mnth])
}

#' Calculate the total catch by year, quarter and gear
#'
#' @param dat A tibble of the catch from gfplot package
#' @param area A vector of regexps like this: c("3[CD]+", "5[CD]+")
#'
#'
#' @return A tibble with the year and sum of landed and discarded catch
#'   by year and quarter.
total.catch.yr.qtr.gear <- function(dat,
                               areas = NULL){
  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  dat <- mutate(dat,
                area = assign_areas(major_stat_area_name,
                                    area_regex = areas)) %>%
    filter(!is.na(area))

  areas <- gsub("\\[|\\]|\\+", "", areas)
  area.num <- substr(areas[1],
                     grep("[0-9]", areas),
                     grep("[0-9]", areas))

  mutate(dat,
         month = month(best_date),
         quarter = case_when(
           month %in% c(1, 2, 3) ~ 4,
           month %in% c(4, 5, 6) ~ 1,
           month %in% c(7, 8, 9) ~ 2,
           month %in% c(10, 11, 12) ~ 3
         )) %>%
    select(-month) %>%
    mutate(year = if_else(quarter == 4, year - 1, as.numeric(year))) %>%
    group_by(year, quarter, gear) %>%
    summarize(
      discarded = sum(discarded_kg) / 1000,
      landed = sum(landed_kg) / 1000,
      catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    rowwise() %>%
    mutate(total_catch = sum(catch_weight)) %>%
    mutate(total_catch = round(total_catch, 3)) %>%
    ## mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
    ungroup()
}

#' Calculate the total catch by year, quarter, gear and vessel
#'
#' @param dat A tibble of the catch from gfplot package
#' @param area A vector of regexps like this: c("3[CD]+", "5[CD]+")
#'
#'
#' @return A tibble with the year and sum of landed and discarded catch
#'   by year and quarter.
total.catch.yr.qtr.gear.vessel <- function(dat,
                                    areas = NULL){
  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  dat <- mutate(dat,
                area = assign_areas(major_stat_area_name,
                                    area_regex = areas)) %>%
    filter(!is.na(area))

  areas <- gsub("\\[|\\]|\\+", "", areas)
  area.num <- substr(areas[1],
                     grep("[0-9]", areas),
                     grep("[0-9]", areas))

  mutate(dat,
         month = month(best_date),
         quarter = case_when(
           month %in% c(1, 2, 3) ~ 4,
           month %in% c(4, 5, 6) ~ 1,
           month %in% c(7, 8, 9) ~ 2,
           month %in% c(10, 11, 12) ~ 3
         )) %>%
    select(-month) %>%
    mutate(year = if_else(quarter == 4, year - 1, as.numeric(year))) %>%
    group_by(year, quarter, gear, vessel_name) %>%
    summarize(
      discarded = sum(discarded_kg) / 1000,
      landed = sum(landed_kg) / 1000,
      catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    rowwise() %>%
    mutate(total_catch = sum(catch_weight)) %>%
    mutate(total_catch = round(total_catch, 3)) %>%
    ## mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
    ungroup()
}

