#The AGE SAMPLE is used to construct the age-length key
# and contains all records with BOTH age and length
#Function filters for surveys in argument, subsets to get columns of interest
get_age_sample <- function(d, surveys=c("SYN HS","SYN QCS"),alkyr) {
 #dplyr::glimpse(d)
 d %>%
    dplyr::filter(is.element(d$survey_abbrev,surveys)) %>%
    dplyr::filter(!is.na(age)) %>%
    dplyr::filter(is.element(year,alkyr)) %>%
    subset(select=c("year","length","age"))
  }
