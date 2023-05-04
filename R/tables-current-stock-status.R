stock.status <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "html",
                           make.lt.gt = TRUE,
                           french=FALSE){
  ## make.lt.gt = add less than and greater than
  ## symbols in table. Changes those columns to character

  model <- model[[1]]

  dat <- as.data.frame(matrix(NA,
                              ncol = 3,
                              nrow = 1))
  if(format == "html"){
    col.names <- c("P(B2023 < LRP)",
                   "P(B2023 < USR)",
                   "P(F2022 > LRR)")

  }else{
    col.names <- c(latex.mlc(c("$P(B_{2023} <$", "$\\mathrm{LRP})$")),
                   latex.mlc(c("$P(B_{2023} <$", "$\\mathrm{USR})$")),
                   latex.mlc(c("$P(F_{2022} >$", "$\\mathrm{LRR})$")))
  }
  if(french==TRUE){
    if(format == "html"){
      col.names <- c("P(B2023 < PRL)",
                     "P(B2023 < RSS)",
                     "P(F2022 > TEL)")

    }else{
      col.names <- c(latex.mlc(c("$P(B_{2023} <$", "$\\mathrm{PRL})$")),
                     latex.mlc(c("$P(B_{2023} <$", "$\\mathrm{RSS})$")),
                     latex.mlc(c("$P(F_{2022} >$", "$\\mathrm{TEL})$")))
    }
  }

  #get probabilities of posterior current stock status (B2023 and F2022)
  d <- as.data.frame(model$mcmccalcs$proj.dat) %>%
    dplyr::filter(TAC==0) %>%  #the TAC is irrelevant for these calcs, not projecting
    mutate(B2023Bmin = B2023/Bmin,
           B2023BAvgS = B2023/BAvgS,
           F2022FAvgS = F2022/FAvgS)

  #1. P(B2023<LRP)
  dat[1, 1] <- f(mean(d$B2023Bmin < 1), 2)

  #2. P(B2023<USR)
  dat[1, 2] <- f(mean(d$B2023BAvgS < 1), 2)

  #3. P(F2022<LRR)
  dat[1, 3] <- f(mean(d$F2022FAvgS > 1), 2)

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

  #colnames(dat) <- col.names

  # Kable not printing right now

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
                       "$B_{\t{2023}}$",
                       "$F_{\t{2022}}$"),
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
