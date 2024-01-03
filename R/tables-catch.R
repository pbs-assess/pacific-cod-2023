catch.table <- function(dat,
                        dat.disc = NULL, # no longer used
                        pre.1996.disc,
                        area = "NA",
                        cap = "",
                        french = FALSE){
  ## dat is what comes out of R/get-data.R/total.catch.yr.qtr
  ## dat.disc is what comes out of R/get-data.R/total.catch.discards

  j <- dat %>%
    group_by(year) %>%
    dplyr::filter(year<2023) %>%
    summarize(Year = year[1],
      USA = sum(usa_catch),
      `Landings` = sum(landed_canada),
      `Released at sea` = sum(discarded_canada)) %>%
    select(-year)

  # Hardwire the pre-1996 discards (pasted from 2018 res doc)
  row1995 <- 1995-1953+1
  pre1996disc <- as.numeric(pre.1996.disc$`released at sea`)
  j$`Released at sea`[1:row1995] <- pre1996disc

  #Calculate total and total catch inlcuding pre-1996 discards
   j <- j %>%
    mutate(`Total` = Landings + `Released at sea`,
           `Total catch` = `Total` + `USA`)

  j <- j[!is.na(j$`Total catch`),]

  j <- j[,c("Year", "Landings", "Released at sea", "Total", "USA", "Total catch")]

  j[,-1] <- round(j[,-1], 1) #they get rounded below anyway

    #export unrounded table for model dat files
    #readr::write_csv(j,here::here("data", paste0("catch_table_",area,".csv")))

    #this also rounds the values
 # j[,c(2,3,4,5,6)] <- apply(j[,c(2,3,4,5,6)],
 #                            2,
 #                            function(x){
 #                              tmp <- as.numeric(x)
 #                              f(tmp)
 #                            })


  if (french) {
    colnames(j) <- c(en2fr(colnames(j)[1], translate = french, allow_missing = TRUE),
      en2fr(colnames(j)[2], translate = french, allow_missing = TRUE, case="lower"),
      en2fr(colnames(j)[3], translate = french, allow_missing = TRUE, case="lower"),
      en2fr(colnames(j)[4], translate = french, allow_missing = TRUE, case="lower"),
      en2fr(colnames(j)[5], translate = french, allow_missing = TRUE),
      en2fr(colnames(j)[6], translate = french, allow_missing = TRUE))
  }

  #Add Canada to colnames for cols 2-4
  # for(k in 2:4){
  #   #colnames(j)[k] <- latex.mlc(c("Canada", colnames(j)[k]))
  #   colnames(j)[k] <- paste(c("Canada", colnames(j)[k]))
  # }

  colnames(j) <- latex.bold(colnames(j))

  # if (french) {
  #   for (i in seq_len(ncol(j))) {
  #     j[,i] <- gsub(",", " ", j[,i,drop=TRUE])
  #     j[,i] <- gsub("\\.", ",", j[,i,drop=TRUE])
  #   }
  # }

  #cut off first three years

  csasdown::csas_table(j[4:nrow(j),], caption = cap, escape = FALSE,
    align = c("l", "r", "r", "r", "r", "r")) %>%
    column_spec(c(2, 4, 5, 6), width = "2cm") %>%
    column_spec(3, width = "4cm") %>%
    kable_styling(latex_options = c("hold_position"))

  # kable(j[4:nrow(j),],
  #       caption = cap,
  #       booktabs = TRUE,
  #       longtable = TRUE,
  #       linesep = "",
  #       escape = FALSE, format = "latex",
  #       align = c("l", "r", "r", "r", "r", "r")) %>%
  #   column_spec(c(2, 4, 5, 6), width = "2cm") %>%
  #   column_spec(3, width = "4cm") %>%
  #   kable_styling(latex_options = c("hold_position"))

}

tac.table <- function(tac,
                      cap = "", french = FALSE){
  ## dat is what comes out of the csv file data/pcod-tac-1996-2018.csv

  names(tac) <- gsub("X", "", names(tac))
  names(tac) <- en2fr(names(tac), translate = french, allow_missing = TRUE)

   tac[,c(2,3,4,5)] <- apply(tac[,c(2,3,4,5)],
                            2,
                            function(x){
                              tmp <- as.numeric(x)
                              f(tmp)
                            })
  tac[grep(" *NA", tac[,2]), 2] = if (!french) "bycatch only" else "prise accessoire"
  tac[grep(" *NA", tac[,3]), 3] = if (!french) "bycatch only" else "prise accessoire"
  tac[grep(" *NA", tac[,4]), 4] = if (!french) "bycatch only" else "prise accessoire"

  #Hardcode the translation for IFMP and bycatch only
  if (french) {
    tac[1:13,6] <- "PGIP"
    tac[23,2:4] <- "prises accessoires"
    for (i in seq_len(ncol(tac))) {
      tac[,i] <- gsub(",", " ", tac[,i,drop=TRUE])
    }
  }

  colnames(tac) <- latex.bold(colnames(tac))

  csasdown::csas_table(tac, caption = cap, escape = FALSE, align = c("l", "r", "r", "r", "r", "l"),
    font_size = 8.5, latex_options = c("hold_position"))
  # kable(tac, caption = cap,
  #       booktabs = TRUE,
  #       longtable = TRUE,
  #       linesep = "",
  #       escape = FALSE, format = "latex",
  #       align = c("l", "r", "r", "r", "r", "l")) %>%
  #   kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 8.5)

}
