#Note that format=pandoc is currently causing errors

decision.table <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "html",
                           tac.vec = NA,
                           make.lt.gt = TRUE,
                           french=FALSE){
  ## make.lt.gt = add less than and greater than
  ## sybols in table. Changes those columns to character

  model <- model[[1]]

  tac <- model$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))
  if(format == "html"){
    col.names <- c("2023 Catch (mt)",
                   "P(B2024 < B2023)",
                   "P(F2023 > F2022)",
                   "P(B2024 < LRP)",
                   "P(B2024 < USR)",
                   "P(F2023 > LRR)")

  }else{
    col.names <- c(latex.mlc(c("$2023$", "$\\mathrm{Catch (mt)}$")),
                   latex.mlc(c("$P(B_{2024} <$", "$B_{2023})$")),
                   latex.mlc(c("$P(F_{2023} >$", "$F_{2022})$")),
                   latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{LRP})$")),
                   latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{USR})$")),
                   latex.mlc(c("$P(F_{2023} >$", "$\\mathrm{LRR})$")))
  }
  if(french==TRUE){
    if(format == "html"){
      col.names <- c("2023 Prise (mt)",
                     "P(B2024 < B2023)",
                     "P(F2023 > F2022)",
                     "P(B2024 < PRL)",
                     "P(B2024 < RSS)",
                     "P(F2023 > TEL)")

    }else{
      col.names <- c(latex.mlc(c("$2023$", "$\\mathrm{Prise (mt)}$")),
                     latex.mlc(c("$P(B_{2024}<$", "$B_{2023})$")),
                     latex.mlc(c("$P(F_{2023} >$", "$F_{2022})$")),
                     latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{PRL})$")),
                     latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{RSS})$")),
                     latex.mlc(c("$P(F_{2023} >$", "$\\mathrm{TEL})$")))
    }
  }


  for(t in seq_along(tac)){
    d <- as.data.frame(model$mcmccalcs$proj.dat)
    d <- d[d$TAC == tac[t],]
    dat[t, 1] <- f(tac[t], 0)
    dat[t, 2] <- f(mean(d$B2024B2023 < 1), 2)
    dat[t, 3] <- f(mean(d$F2023F2022 > 1), 2)
    dat[t, 4] <- f(mean(d$B2024Bmin < 1), 2)
    dat[t, 5] <- f(mean(d$B2024BAvgS < 1), 2)
    dat[t, 6] <- f(mean(d$F2023FAvgS > 1), 2)
  }

  if(make.lt.gt){
    dat <- mutate_at(dat, -1,
                     function(x) gsub('0.00', '<0.01', x))
    dat <- mutate_at(dat, -1,
                     function(x) gsub('1.00', '>0.99', x))
  }

  if (french) {
    for (i in seq_len(ncol(dat))) {
      dat[,i] <- gsub(",", " ", dat[,i])
      dat[,i] <- gsub("\\.", ",", dat[,i])
    }
  }

  if(make.table){
    kable(dat,
          caption = caption,
          booktabs = TRUE,
          longtable = TRUE,
          linesep = "",
          escape = FALSE,
          format = "latex",
          col.names = col.names) %>%
      kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 8.5) %>%
      kableExtra::column_spec(1, width = "2.7cm") %>%
      kableExtra::column_spec(2:6, width = "2.0cm")
  }else{
    dat
  }
}

# 2023: Add a version of the decision table only for a zero catch projection

# Rebuilding trigger guidelines:  It is considered to be at or below its LRP if
# the terminal year stock status indicator is estimated to be at or below the LRP
# with a greater than 50 percent probability OR if the projected stock status
# indicator falls below the LRP with a greater than 50 percent probability under
# a zero catch scenario in a 1-year projection
# https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/precautionary-precaution-eng.htm#toc_2.1

decision.table.zero.tac <- function(model,
                                 caption = "",
                                 make.table = TRUE,
                                 format = "html",
                                 tac.vec = NA,
                                 make.lt.gt = TRUE,
                                 french=FALSE){
  ## make.lt.gt = add less than and greater than
  ## symbols in table. Changes those columns to character

  model <- model[[1]]

  tac <- model$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }

  dat <- as.data.frame(matrix(NA,
                              ncol = 4,
                              nrow = 1))
  if(format == "html"){
    col.names <- c("2023 Catch (mt)",
                   "P(B2024 < B2023)",
                   "P(B2024 < LRP)",
                   "P(B2024 < USR)")

  }else{
    col.names <- c(latex.mlc(c("$2023$", "$\\mathrm{Catch (mt)}$")),
                   latex.mlc(c("$P(B_{2024} <$", "$B_{2023})$")),
                   latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{LRP})$")),
                   latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{USR})$")))
  }
  if(french==TRUE){
    if(format == "html"){
      col.names <- c("2023 Prise (mt)",
                     "P(B2024 < B2023)",
                    "P(B2024 < PRL)",
                     "P(B2024 < RSS)")

    }else{
      col.names <- c(latex.mlc(c("$2023$", "$\\mathrm{Prise (mt)}$")),
                     latex.mlc(c("$P(B_{2024}<$", "$B_{2023})$")),
                     latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{PRL})$")),
                     latex.mlc(c("$P(B_{2024} <$", "$\\mathrm{RSS})$")))
    }
  }

  #get probabilities of posterior current stock status (B2023 and F2022)
  d <- as.data.frame(model$mcmccalcs$proj.dat) %>%
    dplyr::filter(TAC==0)

  dat[1, 1] <- f(tac[1], 0)
  dat[1, 2] <- f(mean(d$B2024B2023 < 1), 2)
  dat[1, 3] <- f(mean(d$B2024Bmin < 1), 2)
  dat[1, 4] <- f(mean(d$B2024BAvgS < 1), 2)


  if(make.lt.gt){
    dat <- mutate_at(dat, -1,
                     function(x) gsub('0.00', '<0.01', x))
    dat <- mutate_at(dat, -1,
                     function(x) gsub('1.00', '>0.99', x))
  }

  if (french) {
    for (i in seq_len(ncol(dat))) {
      dat[,i] <- gsub(",", " ", dat[,i])
      dat[,i] <- gsub("\\.", ",", dat[,i])
    }
  }

  if(make.table){
    kable(dat,
          caption = caption,
          booktabs = TRUE,
          longtable = TRUE,
          linesep = "",
          escape = FALSE,
          format = "latex",
          col.names = col.names) %>%
      kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 8.5) %>%
      kableExtra::column_spec(1, width = "2.7cm") %>%
      kableExtra::column_spec(2:6, width = "2.0cm")
  }else{
    dat
  }
}

suggested.ref.points <- function(french=FALSE, definition_text="definition", caption_text="caption"){

    df <- data.frame(
    referencepoint = c("$B_{\t{Min}}$",
                       "$B_{\t{Avg}}$",
                       "$F_{\t{Avg}}$",
                       "$B_{\t{2022}}$",
                       "$F_{\t{2021}}$"),
    Definition = definition_text,
    Role = c(en2fr("LRP", translate=french, allow_missing=TRUE),
             en2fr("USR", translate=french, allow_missing=TRUE),
             en2fr("LRR", translate=french, allow_missing=TRUE),
             en2fr("Benchmark", translate=french, allow_missing=TRUE),
                   en2fr("Benchmark", translate=french, allow_missing=TRUE))) %>%
    rename("Reference point" = referencepoint)

  colnames(df) <- en2fr(colnames(df), translate = french, allow_missing = TRUE)
  colnames(df) <- latex.bold(colnames(df))
  kable(df,
        caption = caption_text,
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        format = "pandoc",
        align = c("l", "l", "l")) %>%
    column_spec(2, width = "10cm") %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}
