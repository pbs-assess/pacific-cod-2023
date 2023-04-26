priors.table <- function(model,
                         cap = "",
                         basis.vec = NA,
                         digits = 2,
                         french = FALSE){

  ## basis.vec is the reasons for each prior being used.
  ##  if there is a dash - in them, that will signify a new
  ##  line in the cell

  model <- model[[1]]

  p.names <- as.tibble(data.frame(name = c("Uniform",
                                           "Normal",
                                           "Log-Normal",
                                           "Beta",
                                           "Gamma"),
                                  prior = c(0, 1, 2, 3, 4)))

  prior.names <- rownames(model$ctl$params)
  prior.specs <- as.tibble(model$ctl$params) %>%
    transmute(name = prior.names,
              ival = as.character(ival),
              lb = as.character(lb),
              ub = as.character(ub),
              prior = prior,
              p1 = p1,
              p2 = p2,
              Estimated = if_else(phz > 0, en2fr("Yes",translate=french,allow_missing=TRUE), en2fr("No",translate=french,allow_missing=TRUE)))

  ## Add the q parameters to the prior specs table
  q.names <- paste0("q", 1:ncol(model$ctl$surv.q))
  q.specs <- as.tibble(t(model$ctl$surv.q)) %>%
    transmute(name = q.names,
              ival = NA,
              lb = NA,
              ub = NA,
              prior = priortype,
              p1 = priormeanlog,
              p2 = priorsd,
              Estimated = en2fr("Yes",translate=french,allow_missing=TRUE))

  prior.specs <- bind_rows(prior.specs, q.specs)

  ## Remove log part for q's with uniform priors
  non.q <- prior.specs[-grep("q", prior.specs$name),]
  q <- prior.specs[grep("q", prior.specs$name),] %>%
    mutate(name = if_else(prior > 0,
                          paste0("log_", name),
                          name))
  prior.specs <- bind_rows(non.q, q)
  kk <- prior.specs$name[prior.specs$name == "steepness"]

  if(!is.na(basis.vec[1])){
    if(length(basis.vec) != nrow(prior.specs)){
      stop("The basis column vector you supplied is not the same ",
           "length as the number of rows in the priors table. ",
           "The prior names in the table are:\n\n",
           paste(prior.specs$name, collapse = "\n"),
           "\n\nThe basis vector you supplied is:\n\n",
           paste(basis.vec, collapse = "\n"))
    }
  }
  ## Make prior and prior parameters nil for fixed parameters
  prior.specs$ival <- as.numeric(prior.specs$ival)
  prior.specs$lb <- as.numeric(prior.specs$lb)
  prior.specs$ub <- as.numeric(prior.specs$ub)

  prior.specs <- prior.specs %>%
    mutate(prior = if_else(Estimated == "No",
                           NA_real_,
                           prior),
           lb = if_else(Estimated == "No",
                        NA_real_,
                        lb),
           ub = if_else(Estimated == "No",
                        NA_real_,
                        ub),
           p1 = if_else(Estimated == "No",
                        NA_real_,
                        p1),
           p2 = if_else(Estimated == "No",
                        NA_real_,
                        p2))

  prior.specs[prior.specs$name == "steepness", 1] <- "h"

  prior.specs <- prior.specs %>%
    rename(Parameter = name) %>%
    left_join(p.names, by = "prior") %>%
    select(-prior)

  prior.specs <- prior.specs %>%
    transmute(Parameter = Parameter,
              `Initial value` = ival,
              `Lower bound` = lb,
              `Upper bound` = ub,
              Distribution = name,
              P1 = p1,
              P2 = p2,
              Estimated = Estimated)

  if(!is.na(basis.vec[1])){
    ## Replace - with latex multi-line cells

    basis.vec <- sapply(basis.vec,
                        function(x){
                          latex.mlc(unlist(strsplit(x, "-")), FALSE)
                        })

    prior.specs <- prior.specs %>%
      mutate(Basis = basis.vec)
  }

  ## Remove prior information for q's which have Uniform as prior
  prior.specs <- prior.specs %>%
    mutate(pos = 1:nrow(prior.specs))

  q.non.unif <- prior.specs %>%
    filter(grepl("q", Parameter) & Distribution == "Uniform") %>%
    mutate(Distribution = NA,
           P1 = NA,
           P2 = NA)
  the.rest <- prior.specs %>%
    filter(!(grepl("q", Parameter) & Distribution == "Uniform"))

  prior.specs <- bind_rows(the.rest, q.non.unif) %>%
    arrange(pos) %>%
    select(-pos)

  prior.specs$Parameter <- sapply(prior.specs$Parameter,
                                  function(x){
                                    get.rmd.name(x)
                                  })

  ## Round the values off
  prior.specs$`Initial value` <- f(prior.specs$`Initial value`, digits)
  prior.specs$`Upper bound` <- f(prior.specs$`Upper bound`, digits)
  prior.specs$`Lower bound` <- f(prior.specs$`Lower bound`, digits)
  prior.specs$P1 <- f(prior.specs$P1, digits)
  prior.specs$P2 <- f(prior.specs$P2, digits)

  ## Change all NAs to --
  prior.specs <- apply(prior.specs,
             c(1,2),
             function(x){
               if(is.na(x)){
                 "--"
               }else if(length(grep("NA", x))){
                 "--"
               }else{
                 x
               }
             })

  col.names <-  c(latex.bold(en2fr("Parameter", translate = french, allow_missing = TRUE)),
    latex.mlc(en2fr(c("Initial",
                "value"), translate = french, allow_missing = TRUE)),
    latex.mlc(c("Lower", "bound")),
    latex.mlc(c("Upper", "bound")),
    latex.bold(en2fr("Distribution", translate = french, allow_missing = TRUE)),
    latex.bold(en2fr("P1", translate = french, allow_missing = TRUE)),
    latex.bold(en2fr("P2", translate = french, allow_missing = TRUE)),
    latex.bold(en2fr("Estimated", translate = french, allow_missing = TRUE)))

  if(french==TRUE) {
    col.names[3] <- latex.bold(latex.mlc(en2fr(c("Bound", "lower"),translate = french, allow_missing = TRUE)))
    col.names[4] <- latex.bold(latex.mlc(en2fr(c("Bound", "upper"),translate = french, allow_missing = TRUE)))
  }

  if(!is.na(basis.vec[1])){
    col.names <- c(col.names, latex.bold(en2fr("Basis", translate = french, allow_missing = TRUE)))
  }
  colnames(prior.specs) <- col.names

  kable(prior.specs,
        caption = cap, format = "pandoc",
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE,
        align = c("l", "r", "r", "r", "r", "r", "r", "r", "c")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
}
