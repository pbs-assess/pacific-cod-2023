decision.table <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "pandoc",
                           tac.vec = NA,
                           make.lt.gt = TRUE,
                           french=FALSE){
  ## make.lt.gt = add less than and greater than
  ## sybols in table. Changes those columns to character

  model <- model[[1]]

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))
  if(format == "html"){
    col.names <- c("2021 Catch (mt)",
                   "P(B2022 < B2021)",
                   "P(F2021 > F2020)",
                   "P(B2022 < LRP)",
                   "P(B2022 < USR)",
                   "P(F2021 > LRR)")

  }else{
    col.names <- c(latex.mlc(c("$2021$", "$\\mathrm{Catch (mt)}$")),
                   latex.mlc(c("$P(B_{2022} <$", "$B_{2021})$")),
                   latex.mlc(c("$P(F_{2021} >$", "$F_{2020})$")),
                   latex.mlc(c("$P(B_{2022} <$", "$\\mathrm{LRP})$")),
                   latex.mlc(c("$P(B_{2022} <$", "$\\mathrm{USR})$")),
                   latex.mlc(c("$P(F_{2021} >$", "$\\mathrm{LRR})$")))
  }
  if(french==TRUE){
    if(format == "html"){
      col.names <- c("2021 Prise (mt)",
                     "P(B2022 < B2021)",
                     "P(F2021 > F2020)",
                     "P(B2022 < PRL)",
                     "P(B2022 < RSS)",
                     "P(F2021 > TEL)")

    }else{
      col.names <- c(latex.mlc(c("$2021$", "$\\mathrm{Prise (mt)}$")),
                     latex.mlc(c("$P(B_{2022}<$", "$B_{2021})$")),
                     latex.mlc(c("$P(F_{2021} >$", "$F_{2020})$")),
                     latex.mlc(c("$P(B_{2022} <$", "$\\mathrm{PRL})$")),
                     latex.mlc(c("$P(B_{2022} <$", "$\\mathrm{RSS})$")),
                     latex.mlc(c("$P(F_{2021} >$", "$\\mathrm{TEL})$")))
    }
  }

  tac <- model$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }
  for(t in seq_along(tac)){
    d <- as.data.frame(model$mcmccalcs$proj.dat)
    d <- d[d$TAC == tac[t],]
    dat[t, 1] <- f(tac[t], 0)
    dat[t, 2] <- f(mean(d$B2022B2021 < 1), 2)
    dat[t, 3] <- f(mean(d$F2021F2020 > 1), 2)
    dat[t, 4] <- f(mean(d$B2022Bmin < 1), 2)
    dat[t, 5] <- f(mean(d$B2022BAvgS < 1), 2)
    dat[t, 6] <- f(mean(d$F2021FAvgS > 1), 2)
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
