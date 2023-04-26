strip.areas.groups <- function(dat){
  ## This is a hack function to remove the area and group prefixes for the
  ##  mcmc data 'dat'. The reason is that for now we are just working with a
  ##  single group and area, and the extra text in the parameter names are
  ##  confusing, eg. 'ro_gr1' will become just 'ro'. If you make a model with
  ##  more than one group or area this will need to be revisited. Also, this
  ##  removes 'f' which is assumed to be the objective function value. Note
  ##  that q1, q2, q3... will stay the same and m1 and m2 will remain if the
  ##  model was two-sex.

  pnames <- names(dat)
  ## M will only ever be 1 or 2, for each sex
  pnames <- gsub("m_gs1", "m1", pnames)
  pnames <- gsub("m_gs2", "m2", pnames)

  pnames <- gsub("msy1", "msy", pnames)
  pnames <- gsub("fmsy1", "fmsy", pnames)
  pnames <- gsub("SSB1", "ssb", pnames)
  pnames <- gsub("sel_sd([0-9]+)", "selsd\\1", pnames)
  pnames <- gsub("sel_g([0-9]+)", "sel\\1", pnames)
  ## Remove underscores
  names(dat) <- gsub("_+.*", "", pnames)
  ## Remove objective function value
  dat[,names(dat) != "f"]
}

strip.static.params <- function(model, dat){
  ## Strip out the static (non-estimated) parameters from the mcmc output data
  ##  for the given scenario. We only need to see estimated parameters on the
  ##  diagnostic plots. If there are no static parameters, NULL will be returned

  # Check the control file to see which parameters were static
  inp <- as.data.frame(model$ctl$param)
  static <- inp[inp$phz <= 0,]
  snames <- rownames(static)

  ## Now remove those from the mcmc data
  pnames <- names(dat)
  ## remove the log_ stuff from the input parameter names
  snames <- gsub("log_", "", snames)
  ## There will be either one "m" or "m1" and "m2" in pnames.
  ## If "m" is in snames, remove the "m1", and "m2" from pnames as well if they
  ##  exist
  if("m" %in% snames){
    ms <- c("m1", "m2")
    snames <- c(snames, "m1", "m2")
  }
  ## The following also removes "m" in a combined sex model
  dat <- dat[,!(pnames %in% snames)]

  ## Remove static selectivity params
  sel.params <- as.data.frame(model$ctl$sel)
  est.phase <- sel.params["estphase",]
  static.sel <- est.phase < 1
  sel.post.names <- names(dat)[grep("sel[0-9]+",
                                    names(dat))]
  sel.post.names <- sel.post.names[static.sel]
  sel.sd.post.names <- names(dat)[grep("selsd[0-9]+",
                                       names(dat))]
  sel.sd.post.names <- sel.sd.post.names[static.sel]
  dat.names <- names(dat)
  static.sel.inds <- NULL
  if(length(sel.post.names) > 0){
    ## If there are static parameters, remove them
    for(static.sel in 1:length(sel.post.names)){
      static.sel.inds <- c(static.sel.inds,
                           grep(sel.post.names[static.sel],
                              dat.names))
      static.sel.inds <- c(static.sel.inds,
                           grep(sel.sd.post.names[static.sel],
                              dat.names))
    }
    dat <- dat[,-static.sel.inds]
  }
  dat
}
