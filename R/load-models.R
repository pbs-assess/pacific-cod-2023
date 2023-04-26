load.iscam.files <- function(model.dir,
                             burnin = 1000,
                             thin = 1,
                             low = 0.025,
                             high = 0.975,
                             load.proj = TRUE,
                             verbose = FALSE){
  ## Load all the iscam files for output and input, and return the model object
  ## If MCMC directory is present, load that and perform calculations for mcmc
  ##  parameters.
  ##
  ## model.dir - which directory the model is in
  ## burnin - the number of posteriors to remove from the data
  ## thin - the thinning to apply to the posterior samples
  ## low - lower quantile value to apply to mcmc samples
  ## high - higher quantile value to apply to mcmc samples
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables

  curr.func.name <- get.curr.func.name()
  model <- list()
  model$path <- model.dir
  ## Get the names of the input files
  inp.files <- fetch.file.names(model.dir, starter.file.name)
  model$dat.file <- inp.files[[1]]
  model$ctl.file <- inp.files[[2]]
  model$proj.file <- inp.files[[3]]

  ## Load the input files
  model$dat <- read.data.file(model$dat.file)
  model$ctl <- read.control.file(model$ctl.file,
                                 model$dat$ngear,
                                 0)
                                 ## model$dat$num.age.gears)
  model$proj <- read.projection.file(model$proj.file)
  model$par <- read.par.file(file.path(model.dir, par.file))
  ## Load MPD results
  model$mpd <- read.report.file(file.path(model.dir, rep.file))
  ## Some of the parameters need to be logged
  model$mpd <- calc.mpd.logs(model$mpd)
  model.dir.listing <- dir(model.dir)
  ## Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  ## Set the mcmc path. This doesn't mean it exists.
  model$mcmcpath <- model.dir

  model$mcmc <- read.mcmc(model$mcmcpath)
  ## Do the mcmc quantile calculations
  if(length(model$mcmc)){
    model$mcmccalcs <- calc.mcmc(model,
                                 burnin,
                                 thin,
                                 lower = low,
                                 upper = high,
                                 load.proj = load.proj)
    model$mcmc$params <- strip.areas.groups(model$mcmc$params)
    model$mcmc$params <- fix.m(model$mcmc$params)
    model$mcmc$params.est <- as.data.frame(get.estimated.params(model$mcmc$params))
    model$mcmc$params.est <- mcmc.thin(model$mcmc$params.est, burnin, thin)
    model$mcmc$params.est.log <- calc.logs(model$mcmc$params.est)
  }else{
    model$mcmccalcs <- NULL
    model$mcmc$params <- NULL
    model$mcmc$params.est <- NULL
    model$mcmc$params.est.log <- NULL
  }
  class(model) <- model.class
  model
}

delete.rdata.files <- function(models.dir = model.dir){
  ## Delete all rdata files found in the subdirectories of the models.dir
  ## directory.

  dirs <- dir(models.dir)
  dirs <- file.path(models.dir, dirs)
  rdata.files <- file.path(dirs, "*.RData")
  ans <- readline("This operation cannot be undone, are you sure (y/n)? ")
  if(ans == "Y" | ans == "y"){
    ## Delete Retrospectives if they exist
    for(i in seq_along(dirs)){
      subdirs <- dir(dirs[i])
      subdir.names <- subdirs[grep("Retrospective", subdirs)]
      subdirs <- file.path(dirs[i], subdir.names)
      file.names <- file.path(subdirs, paste0(subdir.names, ".Rdata"))
      unlink(file.names, force = TRUE)
    }
    unlink(rdata.files, force = TRUE)
    cat(paste0("Deleted ", rdata.files, "\n"))
    cat("All rdata files were deleted.\n")
  }else{
    cat("No files were deleted.\n")
  }
}

delete.dirs <- function(models.dir = model.dir,
                        sub.dir = NULL){
  ## Delete all directories and files of sub.dir
  ##
  ## models.dir - directory name for all models location
  ## sub.dir - The subdirectory to delete recursively

  dirs <- dir(models.dir)
  files <- file.path(models.dir, dirs, sub.dir)
  unlink(files, recursive = TRUE, force = TRUE)
  cat("All files and directories were deleted from the",
      sub.dir, "directory in each model directory.\n")
}

create.rdata.file <- function(model.dir,
                              ovwrt.rdata = FALSE,
                              load.proj = TRUE,
                              burnin = 1000,
                              thin = 1,
                              low = 0.025,
                              high = 0.975,
                              verbose = FALSE){
  ## Create an rdata file to hold the model's data and outputs.
  ## If an RData file exists, and overwrite is FALSE, return immediately.
  ## If no RData file exists, the model will be loaded from outputs into
  ##  an R list and saved as an RData file in the correct directory.
  ## When this function exits, an RData file will be located in the
  ##  directory given by model.name.
  ## Assumes the files model-setup.r and utilities.r has been sourced.
  ##
  ## models.dir - directory name for all models location
  ## model.name - directory name of model to be loaded
  ## ovwrt.rdata - overwrite the RData file if it exists?
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables
  ## low - lower quantile value to apply to mcmc samples
  ## high - higher quantile value to apply to mcmc samples

  curr.func.name <- get.curr.func.name()
  if(!dir.exists(model.dir)){
    stop(curr.func.name,"Error - the directory ", model.dir,
         " does not exist. Fix the problem and try again.\n")
  }
  ## The RData file will have the same name as the directory it is in
  model.dir.base <- basename(model.dir)
  rdata.file <- file.path(model.dir,
                          paste0(model.dir.base, ".RData"))
  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the RData file
      cat0(curr.func.name, "RData file found in ", model.dir,
           ". Deleting...\n")
      unlink(rdata.file, force = TRUE)
    }else{
      cat0(curr.func.name, "RData file found in ", model.dir, "\n")
      return(invisible())
    }
  }else{
    cat0(curr.func.name, "No RData file found in ", model.dir,
         ". Creating one now.\n")
  }

  ## If this point is reached, no RData file exists so it
  ##  has to be built from scratch
  model <- load.iscam.files(model.dir,
                            burnin = burnin,
                            thin = thin,
                            low = low,
                            high = high,
                            load.proj = load.proj)

  ## Save the model as an RData file
  save(model, file = rdata.file)
  invisible()
}

load.models <- function(model.dir.names){
  ## Load model(s) and return as a list.
  ## Strip last part of path to use as filename
  model.dir.names.base <- basename(model.dir.names)
  model.rdata.files <- file.path(model.dir.names,
                                 paste0(model.dir.names.base,
                                        ".Rdata"))

  out <- lapply(1:length(model.rdata.files),
                function(x){
                  load(model.rdata.files[x])
                  if(class(model) != model.class){
                    model <- list(model)
                  }
                  model
                })
  class(out) <- model.lst.class
  out
}

fetch.file.names <- function(path, ## Full path to the file
                             filename
                             ){
  ## Read the starter file and return a list with 3 elements:
  ## 1. Data file name
  ## 2. Control file name
  ## 3. Projection file name

  ## Get the path the file is in
  d <- readLines(file.path(path, filename), warn = FALSE)
  ## Remove comments
  d <- gsub("#.*", "", d)
  ## Remove trailing whitespace
  d <- gsub(" +$", "", d)
  list(file.path(path, d[1]),
       file.path(path, d[2]),
       file.path(path, d[3]))
}

read.report.file <- function(fn){
  # Read in the data from the REP file given as 'fn'.
  # File structure:
  # It is assumed that each text label will be on its own line,
  # followed by one or more lines of data.
  # If the label is followed by a single value or line of data,
  #  a vector will be created to hold the data.
  # If the label is followed by multiple lines of data,
  #  a matrix will be created to hold the data. The matrix might be
  #  ragged so a check is done ahead of time to ensure correct
  #  matrix dimensions.
  #
  # If a label has another label following it but no data,
  #  that label is thrown away and not included in the returned list.
  #
  # A label must start with an alphabetic character followed by
  # any number of alphanumeric characters (includes underscore and .)

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  idx  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat)
  objs <- dat[idx]     # A vector of the object names
  nobj <- length(objs) # Number of objects
  ret  <- list()
  indname <- 0

  for(obj in 1:nobj){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat,"[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(.ele){c(.ele, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  return(ret)
}

read.data.file <- function(file = NULL,
                           verbose = FALSE){
  # Read in the iscam datafile given by 'file'
  # Parses the file into its constituent parts
  # And returns a list of the contents

  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+","",data)

  # Get the element number for the "Gears" names if present
  dat <- grep("^#.*Gears:.+",data)
  tmp$hasGearNames <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    gearNamesStr <- gsub("^#.*Gears:(.+)","\\1",data[dat])
    gearNames <- strsplit(gearNamesStr,",")[[1]]
    tmp$gearNames <- gsub("^[[:blank:]]+","",gearNames)
    tmp$hasGearNames <- TRUE
  }

  # Get the element number for the "IndexGears" names if present
  ## dat <- grep("^#.*IndexGears:.+",data)
  ## tmp$hasIndexGearNames <- FALSE
  ## if(length(dat >0)){
  ##   # The gear names were in the file
  ##   indexGearNamesStr <- gsub("^#.*IndexGears:(.+)","\\1",data[dat])
  ##   indexGearNames <- strsplit(indexGearNamesStr,",")[[1]]
  ##   tmp$indexGearNames <- gsub("^[[:blank:]]+","",indexGearNames)
  ##   tmp$hasIndexGearNames <- TRUE
  ## }

  ## # Get the element number for the "AgeGears" names if present (gears with age comp data)
  ## dat <- grep("^#.*AgeGears:.+",data)
  ## tmp$hasAgeGearNames <- FALSE
  ## if(length(dat >0)){
  ##   # The gear names were in the file
  ##   ageGearNamesStr <- gsub("^#.*AgeGears:(.+)","\\1",data[dat])
  ##   ageGearNames <- strsplit(ageGearNamesStr,",")[[1]]
  ##   tmp$ageGearNames <- gsub("^[[:blank:]]+","",ageGearNames)
  ##   tmp$hasAgeGearNames <- TRUE
  ## }

  # Get the element number for the "CatchUnits" if present
  dat <- grep("^#.*CatchUnits:.+",data)
  if(length(dat >0)){
    # The catch units comment was in the file
    catchUnitsStr <- gsub("^#.*CatchUnits:(.+)","\\1",data[dat])
    tmp$catchUnits <- gsub("^[[:blank:]]+","",catchUnitsStr)
  }

  # Get the element number for the "IndexUnits" if present
  dat <- grep("^#.*IndexUnits:.+",data)
  if(length(dat >0)){
    # The catch units comment was in the file
    indexUnitsStr <- gsub("^#.*IndexUnits:(.+)","\\1",data[dat])
    tmp$indexUnits <- gsub("^[[:blank:]]+","",indexUnitsStr)
  }

  # Save the number of specimens per year (comment at end of each age comp
  # line), eg. #135 means 135 specimens contributed to the age proportions for that year
  agen <- vector()
  # Match age comp lines which have N's as comments
  tmp$hasAgeCompN <- FALSE
  pattern <- "^[[:digit:]]{4}[[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]].*#([[:digit:]]+).*"
  dat <- data[grep(pattern,data)]
  if(length(dat) > 0){
    for(ageN in 1:length(dat)){
      agen[ageN] <- sub(pattern,"\\1",dat[ageN])
    }
  }
  # N is now a vector of values of N for the age comp data.
  # The individual gears have not yet been parsed out, this will
  # happen later when the age comps are read in.

  # Get the element numbers which start with #.
  dat <- grep("^#.*",data)
  # remove the lines that start with #.
  dat <- data[-dat]

  # remove comments which come at the end of a line
  dat <- gsub("#.*","",dat)

  ## Used in retrospective cases or others where no comments are present,
  if(!length(dat)){
    dat <- data
  }

  # remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+","",dat)
  dat <- gsub("[[:blank:]]+$","",dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the DAT file changes format
  tmp$narea  <- as.numeric(dat[ind <- ind + 1])
  tmp$ngroup <- as.numeric(dat[ind <- ind + 1])
  tmp$nsex   <- as.numeric(dat[ind <- ind + 1])
  tmp$syr    <- as.numeric(dat[ind <- ind + 1])
  tmp$nyr    <- as.numeric(dat[ind <- ind + 1])
  tmp$sage   <- as.numeric(dat[ind <- ind + 1])
  tmp$nage   <- as.numeric(dat[ind <- ind + 1])
  tmp$ngear  <- as.numeric(dat[ind <- ind + 1])

  # Gear allocation
  tmp$alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  if(!tmp$hasGearNames){
    tmp$gearNames <- 1:length(tmp$alloc)
  }

  # Age-schedule and population parameters
  tmp$linf   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$k      <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$to     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lwscal <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lwpow  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age50  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$sd50   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$usemat <- as.numeric(dat[ind <- ind + 1])
  tmp$matvec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  ## Delay-difference options
  tmp$dd.kage    <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.alpha.g <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.rho.g   <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.wk      <- as.numeric(dat[ind <- ind + 1])

  ## Catch data
  tmp$nctobs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch  <- matrix(NA, nrow = tmp$nctobs, ncol = 7)

  for(row in 1:tmp$nctobs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year","gear","area","group","sex","type","value")
  ## Abundance indices are a ragged object and are stored as a list of matrices
  tmp$nit     <- as.numeric(dat[ind <- ind + 1])
  tmp$nitnobs <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$survtype <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  #nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$nit){
    nrows <- tmp$nitnobs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }
  # Age composition data are a ragged object and are stored as a list of matrices
  tmp$nagears     <- as.numeric(dat[ind <- ind + 1])
  #if(!tmp$hasAgeGearNames){
  #  tmp$ageGearNames <- 1:length(tmp$nagears)
  #}

  tmp$nagearsvec  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$nagearssage <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$nagearsnage <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff         <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$agecompflag <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$agecomps    <- NULL
  # one list element for each gear (tmp$nagears)
  if(tmp$nagearsvec[1] > 0){ # Check to see if there are age comp data
   tmp$agecomps <- list()
   for(gear in 1:tmp$nagears){
     nrows <- tmp$nagearsvec[gear]
     ncols <- tmp$nagearsnage[gear] - tmp$nagearssage[gear] + 6 # 5 of the 6 here is for the header columns
     tmp$agecomps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)
     for(row in 1:nrows){
       tmp$agecomps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
     }
     colnames(tmp$agecomps[[gear]]) <- c("year","gear","area","group","sex",tmp$nagearssage[gear]:tmp$nagearsnage[gear])
   }
  }
  ## Build a list of age comp gear N's
  tmp$agearsN <- list()
  start <- 1
  for(ng in 1:length(tmp$nagearsvec)){
    end <- start + tmp$nagearsvec[ng] - 1
    tmp$agearsN[[ng]] <- agen[start:end]
    start <- end + 1
  }
  ## Empirical weight-at-age data
  tmp$nwttab <- as.numeric(dat[ind <- ind + 1])
  tmp$nwtobs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$nwtobs > 0){
    # Parse the weight-at-age data
    nrows       <- tmp$nwtobs
    ncols       <- tmp$nage - tmp$sage + 6
    tmp$waa <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$waa[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$waa) <- c("year","gear","area","group","sex",tmp$sage:tmp$nage)
   }

   # Annual Mean Weight data
    # Catch data
    tmp$nmeanwt <- as.numeric(dat[ind <- ind + 1])
    tmp$nmeanwtobs <- as.numeric(dat[ind <- ind + 1])
    if(tmp$nmeanwtobs >0){
	    tmp$meanwtdata  <- matrix(NA, nrow = sum(tmp$nmeanwtobs), ncol = 7)
	    for(row in 1:sum(tmp$nmeanwtobs)){
	      tmp$meanwtdata[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
	    }
	    colnames(tmp$meanwtdata) <- c("year","meanwt","gear","area","group","sex","timing")
   }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])

  return(tmp)
}

read.control.file <- function(file = NULL,
                              num.gears = NULL,
                              num.age.gears = NULL,
                              verbose = FALSE){
  ## Read in the iscam control file given by 'file'
  ## Parses the file into its constituent parts and returns a list of the
  ##  contents.
  ## num.gears is the total number of gears in the datafile
  ## num.age.gears in the number of gears with age composition information in the
  ##  datafile

  curr.func <- get.curr.func.name()
  if(is.null(num.gears)){
    cat0(curr.func,
         "You must supply the total number of gears (num.gears). ",
         "Returning NULL.")
    return(NULL)
  }
  if(is.null(num.age.gears)){
    cat0(curr.func,
         "You must supply the number of gears with age composition ",
         "(num.age.gears). Returning NULL.")
    return(NULL)
  }

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## Remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Save the parameter names, since they are comments and will be deleted in
  ##  subsequent steps.
  ## To get the npar, remove any comments and preceeding and trailing
  ##  whitespace for it.
  dat1 <- gsub("#.*", "", dat[1])
  dat1 <- gsub("^[[:blank:]]+", "", dat1)
  dat1 <- gsub("[[:blank:]]+$", "", dat1)
  n.par <- as.numeric(dat1)
  param.names <- vector()
  ## Lazy matching with # so that the first instance matches, not any other
  pattern <- "^.*?# *([[:alnum:]]+[_.]*[[:alnum:]]*).*"
  for(param.name in 1:n.par){
    ## Each parameter line in dat which starts at index 2,
    ##  retrieve the parameter name for that line
    param.names[param.name] <- sub(pattern, "\\1", dat[param.name + 1])
  }
  ## Replace any periods in the names with underscores
  param.names <- gsub("[.]", "_", param.names)

  ## Now that parameter names are stored, parse the file.
  ##  remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the CTL file and needs to
  ## be updated whenever the CTL file changes format.
  tmp <- list()
  ind <- 0
  tmp$num.params <- as.numeric(dat[ind <- ind + 1])
  tmp$params <- matrix(NA, nrow = tmp$num.params, ncol = 7)
  for(param in 1:tmp$num.params){
    tmp$params[param,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$params) <- c("ival","lb","ub","phz","prior","p1","p2")
  ## param.names is retreived at the beginning of this function
  rownames(tmp$params) <- param.names

  ## Age and size composition control parameters and likelihood types
  nrows <- 8
  ncols <- num.age.gears
  tmp$age.size <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$age.size[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$age.size) <- c("gearind",
                              "likelihoodtype",
                              "minprop",
                              "comprenorm",
                              "logagetau2phase",
                              "phi1phase",
                              "phi2phase",
                              "degfreephase")
  ## Ignore the int check value
  ind <- ind + 1

  ## Selectivity parameters for all gears
  nrows <- 10
  ncols <- num.gears
  tmp$sel <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$sel[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$sel) <- c("iseltype",
                         "agelen50log",
                         "std50log",
                         "nagenodes",
                         "nyearnodes",
                         "estphase",
                         "penwt2nddiff",
                         "penwtdome",
                         "penwttvs",
                         "nselblocks")

  ## Start year for time blocks, one for each gear
  max.block <- max(tmp$sel[10,])
  tmp$start.yr.time.block <- matrix(nrow = num.gears, ncol = max.block)
  for(ng in 1:num.gears){
    ## Pad the vector with NA's to make it the right size if it isn't
    ##  maxblocks size.
    tmp.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    if(length(tmp.vec) < max.block){
      for(i in (length(tmp.vec) + 1):max.block){
        tmp.vec[i] <- NA
      }
    }
    tmp$start.yr.time.block[ng,] <- tmp.vec
  }

  ## Priors for survey Q, one column for each survey
  tmp$num.indices <- as.numeric(dat[ind <- ind + 1])
  nrows <- 3
  ncols <- tmp$num.indices
  tmp$surv.q <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$surv.q[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$surv.q) <- c("priortype",
                            "priormeanlog",
                            "priorsd")

  ## Controls for fitting to mean weight data
  tmp$fit.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.cv <- as.numeric(dat[ind <- ind + 1])
  n.vals <- tmp$num.mean.weight.cv
  tmp$weight.sig <-  vector(length = n.vals)
  for(val in 1:n.vals){
    tmp$weight.sig[val] <- as.numeric(dat[ind <- ind + 1])
  }

  ## Miscellaneous controls
  n.rows <- 16
  tmp$misc <- matrix(NA, nrow = n.rows, ncol = 1)
  for(row in 1:n.rows){
    tmp$misc[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$misc) <- c("verbose",
                          "rectype",
                          "sdobscatchfirstphase",
                          "sdobscatchlastphase",
                          "unfishedfirstyear",
                          "maternaleffects",
                          "meanF",
                          "sdmeanFfirstphase",
                          "sdmeanFlastphase",
                          "mdevphase",
                          "sdmdev",
                          "mnumestnodes",
                          "fracZpriorspawn",
                          "agecompliketype",
                          "IFDdist",
                          "fitToMeanWeight")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

read.projection.file <- function(file = NULL,
                                 verbose = FALSE){
  ## Read in the projection file given by 'file'
  ## Parses the file into its constituent parts
  ##  and returns a list of the contents

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## remove the lines that start with #.
  if(length(dat)){
    dat <- data[-dat]
  }else{
    dat <- data
  }

  ## remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the proj file changes format.
  tmp <- list()
  ind <- 0

  ## Get the TAC values
  tmp$num.tac  <- as.numeric(dat[ind <- ind + 1])
  for(tac in 1:tmp$num.tac){
    ## Read in the tacs, one, per line
    tmp$tac.vec[tac] <- as.numeric(dat[ind <- ind + 1])
  }

  ## If the tac vector is on one line
  ##tmp$tac.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  ## Get the control options vector
  tmp$num.ctl.options <- as.numeric(dat[ind <- ind + 1])
  n.rows <- tmp$num.ctl.options
  n.cols <- 1
  tmp$ctl.options  <- matrix (NA, nrow = n.rows, ncol = n.cols)
  for(row in 1:n.rows){
    tmp$ctl.options[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  or it here.
  option.names <- c("syrmeanm",
                    "nyrmeanm",
                    "syrmeanfecwtageproj",
                    "nyrmeanfecwtageproj",
                    "syrmeanrecproj",
                    "nyrmeanrecproj",
                    "shortcntrlpts",
                    "longcntrlpts",
                    "bmin")
  rownames(tmp$ctl.options) <- option.names[1:tmp$num.ctl.options]
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

read.par.file <- function(file = NULL,
                          verbose = FALSE){
  ## Read in the parameter estimates file given by 'file'
  ## Parses the file into its constituent parts
  ## And returns a list of the contents

  data <- readLines(file, warn = FALSE)
  tmp <- list()
  ind <- 0

  ## Remove preceeding #
  conv.check <- gsub("^#[[:blank:]]*", "", data[1])
  ## Remove all letters, except 'e'
  ##convCheck <- gsub("[[:alpha:]]+","",convCheck)
  convCheck <- gsub("[abcdfghijklmnopqrstuvwxyz]",
                    "",
                    conv.check,
                    ignore.case = TRUE)
  ## Remove the equals signs
  conv.check <- gsub("=", "", conv.check)
  ## Remove all preceeding and trailing whitespace
  conv.check <- gsub("^[[:blank:]]+", "", conv.check)
  conv.check <- gsub("[[:blank:]]+$", "", conv.check)
  ## Remove the non-numeric parts
  conv.check <- strsplit(conv.check, " +")[[1]]
  conv.check <- conv.check[grep("^-?[[:digit:]]", conv.check)]
  ## The following values are saved for appending to the tmp list later

  num.params   <- conv.check[1]
  obj.fun.val <-  format(conv.check[2], digits = 6, scientific = FALSE)
  max.gradient <-  format(conv.check[3], digits = 8, scientific = FALSE)

  ##Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  ## At this point, every odd line is a comment and every even line is the value.
  ## Parse the names from the odd lines (oddData) and parse the
  ## values from the even lines (evenData)
  odd.elem <- seq(1, length(data), 2)
  even.elem <- seq(2, length(data), 2)
  odd.data <- data[odd.elem]
  even.data <- data[even.elem]

  ## Remove preceeding and trailing whitespace if it exists from both
  ##  names and values.
  names <- gsub("^[[:blank:]]+", "", odd.data)
  names <- gsub("[[:blank:]]+$", "", names)
  values <- gsub("^[[:blank:]]+", "", even.data)
  values <- gsub("[[:blank:]]+$", "", values)

  ## Remove the preceeding # and whitespace and the trailing : from the names
  pattern <- "^#[[:blank:]]*(.*)[[:blank:]]*:"
  names <- sub(pattern, "\\1", names)

  ## Remove any square brackets from the names
  names <- gsub("\\[|\\]", "", names)

  data.length <- length(names)
  for(item in 1:(data.length)){
    tmp[[item]] <-
      as.numeric(strsplit(values[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }

  names(tmp) <- names
  tmp$num.params <- num.params
  tmp$obj.fun.val <- as.numeric(obj.fun.val)
  tmp$max.gradient <- as.numeric(max.gradient)
  tmp
}

read.mcmc <- function(model.dir = NULL,
                      verbose = TRUE){
  ## Read in the MCMC results from an iscam model run found in the directory
  ##  model.dir.
  ## Returns a list of the mcmc outputs, or NULL if there was a problem or
  ##  there are no MCMC outputs.

  curr.func <- get.curr.func.name()
  if(is.null(model.dir)){
    cat0(curr.func,
         "You must supply a directory name (model.dir). Returning NULL.")
    return(NULL)
  }
  mcmcfn     <- file.path(model.dir, mcmc.file)
  mcmcsbtfn  <- file.path(model.dir, mcmc.biomass.file)
  mcmcrtfn   <- file.path(model.dir, mcmc.recr.file)
  mcmcrdevfn <- file.path(model.dir, mcmc.recr.devs.file)
  mcmcftfn   <- file.path(model.dir, mcmc.fishing.mort.file)
  mcmcutfn   <- file.path(model.dir, mcmc.fishing.mort.u.file)
  mcmcvbtfn  <- file.path(model.dir, mcmc.vuln.biomass.file)
  mcmcprojfn <- file.path(model.dir, mcmc.proj.file)

  tmp        <- list()
  if(file.exists(mcmcfn)){
    tmp$params <- read.csv(mcmcfn)
  }
  if(file.exists(mcmcsbtfn)){
    sbt        <- read.csv(mcmcsbtfn)
    tmp$sbt    <- extract.group.matrices(sbt, prefix = "sbt")
  }
  if(file.exists(mcmcrtfn)){
    rt         <- read.csv(mcmcrtfn)
    tmp$rt     <- extract.group.matrices(rt, prefix = "rt")
  }
  if(file.exists(mcmcftfn)){
    ft         <- read.csv(mcmcftfn)
    tmp$ft     <- extract.area.sex.matrices(ft, prefix = "ft")
  }
  if(file.exists(mcmcutfn)){
    ut         <- read.csv(mcmcutfn)
    tmp$ut     <- extract.area.sex.matrices(ut, prefix = "ut")
  }
  if(file.exists(mcmcrdevfn)){
    rdev       <- read.csv(mcmcrdevfn)
    tmp$rdev   <- extract.group.matrices(rdev, prefix = "rdev")
  }
  if(file.exists(mcmcvbtfn)){
    vbt        <- read.csv(mcmcvbtfn)
    tmp$vbt    <- extract.area.sex.matrices(vbt, prefix = "vbt")
  }
  tmp$proj <- NULL
  if(file.exists(mcmcprojfn)){
    tmp$proj   <- read.csv(mcmcprojfn)
  }
  tmp
}

extract.group.matrices <- function(data = NULL,
                                   prefix = NULL){
  ## Extract the data frame given (data) by unflattening into a list of matrices
  ## by group. The group number is located in the names of the columns of the
  ## data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
  ## or more digits representing the group number and prefix is the string
  ## given as an argument to the function.
  ## Returns a list of matrices, one element per group.

  curr.func.name <- get.curr.func.name()
  if(is.null(data) || is.null(prefix)){
    cat0(curr.func.name,
         "You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a specific
    ##  pattern for it
    group.pattern <- paste0(prefix, group, "_[[:digit:]]+")
    group.names   <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern      <- paste0(prefix, "[[:digit:]]+_([[:digit:]]+)")
    group.names   <- sub(pattern, "\\1", group.names)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[,grep(group.pattern, names)]
    colnames(dat) <- group.names
    tmp[[group]]  <- dat
  }
  tmp
}

extract.area.sex.matrices <- function(data = NULL,
                                      prefix = NULL){
  ## Extract the data frame given (data) by unflattening into a list of matrices
  ##  by area-sex and gear. The area-sex number is located in the names of the
  ##  columns of the data frame in this format:
  ##  "prefix[areasexnum]_gear[gearnum]_year" where [areasexnum] and [gearnum]
  ##  are one or more digits and prefix is the string given as an argument
  ##  to the function.
  ## Returns a list (area-sex) of lists (gears) of matrices, one element
  ##  per group.

  curr.func.name <- get.curr.func.name()
  if(is.null(data) || is.null(prefix)){
    cat0(curr.func.name,
         "You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_gear[[:digit:]]+_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a
    ##  specific pattern for it
    group.pattern <- paste0(prefix, group, "_gear[[:digit:]]+_[[:digit:]]+")
    group.names <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_gear([[:digit:]]+_[[:digit:]]+)")
    group.names <- sub(pattern, "\\1", group.names)
    ## At this point, group.names' elements look like this: 1_1963
    ## The first value is the gear, and the second, the year.
    ## Get the unique gears for this area-sex group
    pattern <- "([[:digit:]]+)_[[:digit:]]+"
    gears <- sub(pattern, "\\1", group.names)
    unique.gears <- unique(as.numeric(gears))
    tmp2 <- vector("list", length = length(unique.gears))
    for(gear in 1:length(unique.gears)){
      gear.pattern <- paste0(prefix, group,"_gear", gear, "_[[:digit:]]+")
      ## Now, the data must be extracted
      ## Get the column numbers that this group are included in
      dat <- data[,grep(gear.pattern, names)]
      ##colnames(dat) <- groupNames
      tmp2[[gear]] <- dat
    }
    tmp[[group]] <- tmp2
  }
  tmp
}

mcmc.thin <- function(mcmc.dat,
                      burnin,
                      thin){
  ## Apply burnin and thinning to the data

  nm <- names(mcmc.dat)
  mcmc.obj <- apply(mcmc.dat, 2, mcmc)
  mcmc.window <- NULL
  for(col in 1:ncol(mcmc.obj)){
    tmp <- window(mcmc.obj[,col],
                  start = burnin + 1,
                  thin = thin)
    mcmc.window <- cbind(mcmc.window, tmp)
  }
  mcmc.window <- as.data.frame(mcmc.window)
  names(mcmc.window) <- nm
  mcmc.window
}

calc.mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      lower = 0.025,
                      upper = 0.975,
                      load.proj = TRUE){
  ## Do the mcmc calculations, i.e. quantiles for parameters
  ## Returns a list of them all
  ##
  ## mcmc - output of the read.mcmc function
  ## burnin - the number of posteriors to remove from the data
  ## thin - the thinning to apply to the posterior samples
  ## lower - lower quantile for confidence interval calcs
  ## upper - upper quantile for confidence interval calcs
  ## load.proj - load the projections from the mcmc and do the calculations
  ##  to construct the decision tables

  curr.func.name <- get.curr.func.name()
  if(is.null(mcmc)){
    cat0(curr.func.name,
         "The mcmc list was null. Check read.mcmc function. Returning NULL.")
    return(NULL)
  }

  probs <- c(lower, 0.5, upper)

  ## Parameters
  mc <- model$mcmc
  mpd <- model$mpd
  params.dat <- mc$params
  params.dat <- strip.areas.groups(params.dat)
  params.dat <- strip.static.params(model, params.dat)
  nm <- names(params.dat)

  p.dat <- params.dat[ , -which(nm %in% c("msy",
                                          "fmsy",
                                          "bmsy",
                                          "umsy",
                                          "ssb",
                                          "bo"))]
  p.dat <- fix.m(p.dat)
  p.dat <- mcmc.thin(p.dat, burnin, thin)
  p.dat.log <- calc.logs(p.dat)
  p.quants <- apply(p.dat, 2, quantile, prob = probs)
  p.quants.log <- apply(p.dat.log, 2, quantile, prob = probs)

  ## Reference points
  r.dat <- params.dat[ , which(nm %in% c("bo",
                                         "bmsy",
                                         "msy",
                                         "fmsy",
                                         "umsy"))]
  r.dat <- mcmc.thin(r.dat, burnin, thin)

  ## Spawning biomass
  sbt.dat <- mcmc.thin(mc$sbt[[1]], burnin, thin)
  sbt.quants <- apply(sbt.dat,
                      2,
                      quantile,
                      prob = probs)
  sbt.quants <- rbind(sbt.quants, mpd$sbt)
  rownames(sbt.quants)[4] <- "MPD"

  ## Depletion
  depl.dat <- apply(sbt.dat,
                    2,
                    function(x){x / r.dat$bo})
  depl.quants <- apply(sbt.dat / r.dat$bo,
                       2,
                       quantile,
                       prob = probs)
  depl.quants <- rbind(depl.quants, mpd$sbt / mpd$bo)
  rownames(depl.quants)[4] <- "MPD"

  ## Recruitment
  recr.dat <- mcmc.thin(mc$rt[[1]], burnin, thin)
  recr.mean <- apply(recr.dat,
                     2,
                     mean)
  recr.quants <- apply(recr.dat,
                       2,
                       quantile,
                       prob = probs)
  recr.quants <- rbind(recr.quants, mpd$rt)
  rownames(recr.quants)[4] <- "MPD"

  ## Recruitment deviations
  recr.devs.dat <- mcmc.thin(mc$rdev[[1]], burnin, thin)
  recr.devs.quants <- apply(recr.devs.dat,
                            2,
                            quantile,
                            prob = probs)

  ## Q for the survey indices
  q.dat <- p.dat[, grep("^q[[:digit:]]+$", colnames(p.dat))]
  num.indices <- ncol(q.dat)
  g.nms <- model$dat$gear.names
  colnames(q.dat) <- g.nms[(length(g.nms) - num.indices + 1):
                           length(g.nms)]
  q.quants <- apply(q.dat,
                    2,
                    quantile,
                    prob = probs)

  build.quant.list <- function(mc.dat, mpd.dat){
    ## Run quantiles on each dataframe in a list of dataframes and append
    ##  the MPD values as well. Returns a list of dataframes
    quants <- lapply(mc.dat,
                     function(x){
                       apply(x,
                             2,
                             quantile,
                             prob = probs,
                             na.rm = TRUE)})
    lapply(1:length(quants),
           function(x){
             quants[[x]] <- rbind(quants[[x]], mpd.dat[x,])
             rownames(quants[[x]])[4] <- "MPD"
             c.names <- colnames(quants[[x]])
             colnames(quants[[x]]) <-
               gsub("^.*_([[:digit:]]+$)", "\\1", c.names)
             quants[[x]]
           })
  }
  ## Vulnerable biomass by gear (list of data frames)
  vuln.dat <- lapply(mc$vbt[[1]], mcmc.thin, burnin, thin)
  ## Reshape the vulnerable biomass output from the MPD
  vbt <- as.data.frame(mpd$vbt)
  vbt <- split(vbt, vbt[,1])
  vbt <- lapply(1:length(vbt),
              function(x){
                vbt[[x]][,4]})
  vbt <- do.call(rbind, vbt)
  vuln.quants <- build.quant.list(vuln.dat, vbt)

  ## Fishing mortalities by gear (list of data frames)
  f.mort.dat <- lapply(mc$ft[[1]], mcmc.thin, burnin, thin)
  f.mort.quants <- build.quant.list(f.mort.dat, mpd$ft)

  u.mort.dat <- lapply(mc$ut[[1]], mcmc.thin, burnin, thin)
  u.mort.quants <- build.quant.list(u.mort.dat, mpd$ut)

  ## Add calculated reference points - these have already been thinned
  ##  and burned in
  sbt.yrs <- names(sbt.dat)
  sbt.init <- sbt.dat[,1]
  sbt.end <- sbt.dat[,ncol(sbt.dat)]
  yr.sbt.init <- sbt.yrs[1]
  yr.sbt.end <- sbt.yrs[length(sbt.yrs)]
  f.yrs <- names(f.mort.dat[[1]])
  f.yrs <- gsub(".*_([[:digit:]]+)",
                 "\\1",
                 f.yrs)
  f.end <- f.mort.dat[[1]][,ncol(f.mort.dat[[1]])]
  yr.f.end <- f.yrs[length(f.yrs)]
  r.dat <- cbind(r.dat,
                 sbt.init,
                 sbt.end,
                 sbt.end / r.dat$bo,
                 sbt.end / sbt.init,
                 f.end,
                 0.4 * r.dat$bmsy,
                 0.8 * r.dat$bmsy)
  names(r.dat) <- c("bo",
                    "bmsy",
                    "msy",
                    "fmsy",
                    "umsy",
                    paste0("b", yr.sbt.init),
                    paste0("b", yr.sbt.end),
                    paste0("b", yr.sbt.end, "/bo"),
                    paste0("b", yr.sbt.end, "/", yr.sbt.init),
                    paste0("f", yr.f.end),
                    paste0("0.4bmsy"),
                    paste0("0.8bmsy"))

  r.quants <- apply(r.dat, 2, quantile, prob = probs)

  desc.col <- c(latex.subscr.ital("B", "0"),
                latex.subscr.ital("B", "MSY"),
                "MSY",
                latex.subscr.ital("F", "MSY"),
                latex.subscr.ital("U", "MSY"),
                latex.subscr.ital("B", yr.sbt.init),
                latex.subscr.ital("B", yr.sbt.end),
                paste0(latex.subscr.ital("B", yr.sbt.end),
                       "/",
                       latex.subscr.ital("B", 0)),
                paste0(latex.subscr.ital("B", yr.sbt.end),
                       "/",
                       latex.subscr.ital("B", yr.sbt.init)),
                latex.subscr.ital("F", yr.f.end),
                paste0("0.4",
                       latex.subscr.ital("B", "MSY")),
                paste0("0.8",
                       latex.subscr.ital("B", "MSY")))

  r.quants <- t(r.quants)
  r.quants <- cbind.data.frame(desc.col, r.quants)
  col.names <- colnames(r.quants)
  col.names <- latex.bold(latex.perc(col.names))
  col.names[1] <- latex.bold("Reference Point")
  colnames(r.quants) <- col.names

  proj.dat <- NULL
  if(load.proj){
    tac <- model$proj$tac.vec
    for(t in seq_along(tac)){
      proj.dat <- rbind(proj.dat,
                        mcmc.thin(filter(model$mcmc$proj,
                                         TAC == tac[t]),
                                  burnin = burnin,
                                  thin = thin))
    }
  }

  sapply(c("p.dat",
           "p.quants",
           "p.dat.log",
           "p.quants.log",
           "r.dat",
           "r.quants",
           "sbt.dat",
           "sbt.quants",
           "depl.dat",
           "depl.quants",
           "recr.dat",
           "recr.quants",
           "recr.devs.dat",
           "recr.devs.quants",
           "q.dat",
           "q.quants",
           "vuln.dat",
           "vuln.quants",
           "f.mort.dat",
           "f.mort.quants",
           "u.mort.dat",
           "u.mort.quants",
           "proj.dat"),
           function(x){get(x)})
}

get.estimated.params <- function(mc){
  ## Return a data frame of estimated parameters, based on the values of
  ##  the posteriors. If all are different, a parameter has been estimated.

  posts <- apply(mc,
                 2,
                 function(x){
                   if(length(unique(x)) > 1)
                     return(x)
                 })
  ## Remove NULL list elements (fixed parameters)
  posts.lst <- posts[!sapply(posts, is.null)]
  do.call(cbind, posts.lst)
}

calc.logs <- function(mc,
                      log.params = c("^ro$",
                                     "^m$",
                                     "^m1$",
                                     "^m2$",
                                     "^rbar$",
                                     "^rinit$",
                                     "^q[1-9]+$")){
  ## Returns a data frame with the logs calculated for various parameters.
  ## The column names will be prepended with log_ for those parameters.
  ##
  ## log.params - the names of the parameters to perform log function.
  ##  Use regular expressions for things like q to represent q1, q2, ... q10

  nm <- colnames(mc)
  grp <- lapply(log.params,
                function(x){
                  grep(x, nm)})
  inds.lst <- grp[sapply(grp,
                         function(x){
                           length(x) > 0})]
  inds <- unique(do.call(c, inds.lst))
  colnames(mc)[inds] <- paste0("log_", colnames(mc)[inds])
  mc[,inds] <- apply(mc[,inds],
                     2,
                     log)
  mc
}

fix.m <- function(mc){
  ## Returns a data frame the same as mc with the following modifications:
  ## If m1 and m2 both exist, no change
  ## If only m1 exists, change the name to m

  grp <- grep("m[12]", colnames(mc))
  if(length(grp) == 1){
    colnames(mc)[grp] <- "m"
  }
  mc
}

calc.mpd.logs <- function(mpd,
                          log.params = c("^ro$",
                                         "^m$",
                                         "^m1$",
                                         "^m2$",
                                         "^rbar$",
                                         "^rinit$",
                                         "^q$")){
  ## Returns a the mpd data list with some parameters logged
  ##  and added to the list
  ##
  ## log.params - the names of the parameters to log. The new values
  ##  will have the same names but prepended with log_

  grp <- lapply(log.params,
                function(x){
                  grep(x, names(mpd))})
  inds.lst <- grp[sapply(grp,
                         function(x){
                           length(x) > 0})]
  inds <- unique(do.call(c, inds.lst))
  log.names <- paste0("log_", names(mpd)[inds])
  vals <- lapply(mpd[inds], log)
  names(vals) <- log.names
  c(mpd, vals)
}
