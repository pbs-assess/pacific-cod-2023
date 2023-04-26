get.cpue <- function(ar = "5ABCD"){
  ## Get the CPUE time series for post-1995 data and place into correct iscam dat file format
  ## This is due to incorrect localities being used in the original series

  if(ar == "5ABCD"){
    gear <- 6
  }else{
    gear <- 4
  }
  readr::read_csv(file.path(rootd.data, "cpue-2018-09-06.csv")) %>%
    filter(formula_version == "Full standardization") %>%
    select(area, year, est, se_link) %>%
    mutate(weight = 1 / se_link) %>%
    select(-se_link) %>%
    mutate(est = round(est, 2), weight = round(weight, 2)) %>%
    filter(area == ar) %>%
    mutate(iyr = year, it = est, gear = gear, area = 1, group = 1, sex = 0, wt = weight, timing = 0) %>%
    select(iyr, it, gear, area, group, sex, wt, timing) %>%
    as.tibble()
}

write.data.file <- function(file = NULL,
                            d,
                            new.cpue = NULL,
                            retro = 0){

  endyr <- d$nyr - retro
  str <- paste(d$narea,
               d$ngroup,
               d$nsex,
               d$syr,
               endyr,
               d$sage,
               d$nage,
               d$ngear,
               paste(d$alloc, collapse = " "),

               d$linf,
               d$k,
               d$to,
               d$lwscal,
               d$lwpow,
               d$age50,
               d$sd50,
               d$usemat,
               d$matvec, ## Note this could be a vector in which case this will break

               d$dd.kage,
               d$dd.alpha.g,
               d$dd.rho.g,
               d$dd.wk,

               nrow(d$catch[d$catch[,1] <= endyr,]),
               "\n",
               sep = "\n")
  cat(str,
      file = file,
      append = FALSE)
  write.table(d$catch[d$catch[,1] <= endyr,],
              file = file,
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE)

  cat(d$nit,
      "\n",
      file = file,
      append = TRUE)

  ## Indices data. First have to determine how many are to going to be removed,
  ##  to write the total number to the file, then remove them and write the data
  for(i in 1:d$nit){
    x <- d$indices[[i]]
    num <- nrow(x[x[,1] <= endyr,])
    cat(num, " ",
        file = file,
        append = TRUE)
  }
  cat0("\n",
       paste(d$survtype, collapse = " "),
       "\n",
       file = file,
       append = TRUE)

  for(i in 1:d$nit){
    x <- as.tibble(d$indices[[i]])
    ## Fix erroneous post-1995 CPUE
    if(x$gear[1] == new.cpue$gear[1]){
      x = new.cpue
    }

    write.table(x[x[,1] <= endyr,],
                file = file,
                append = TRUE,
                col.names = FALSE,
                row.names = FALSE)
  }
  str <- paste(d$nagears,
               paste(d$nagearsvec, collapse = " "),
               d$nagearssage,
               d$nagearsnage,
               d$eff,
               d$agecompflag,
               ## d$agecomps, ## Note this will break if d$agecompflag != 1
               ## This is where the age comp data should go if it exists
               d$nwttab,
               d$nwtobs,
               d$nmeanwt,
               "\n",
               sep = "\n")
  cat(str,
      file = file,
      append = TRUE)

  cat(nrow(d$meanwtdata[d$meanwtdata[,1] <= endyr - 1,]),
      "\n",
      file = file,
      append = TRUE)

  write.table(d$meanwtdata[d$meanwtdata[,1] <= endyr - 1,],
              file = file,
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE)

  cat(999,
      "\n",
      file = file,
      append = TRUE)
}

sens.models.names.list <- c(unlist(sens.models.dir.name.1),
                            unlist(sens.models.dir.name.2),
                            unlist(sens.models.dir.name.3),
                            unlist(sens.models.dir.name.4),
                            unlist(sens.models.dir.name.5),
                            unlist(sens.models.dir.name.6),
                            unlist(sens.models.dir.name.7),
                            unlist(sens.models.dir.name.8),
                            unlist(sens.models.dir.name.9),
                            unlist(sens.models.dir.name.10),
                            unlist(sens.models.dir.name.11),
                            unlist(sens.models.dir.name.12),
                            unlist(sens.models.dir.name.13),
                            unlist(sens.models.dir.name.14))

dat.files <- file.path(sens.models.names.list, "pcod.dat")

lapply(dat.files,
       function(x){
         d <- read.data.file(x)
         if(length(grep("5ABCD", x))){
           write.data.file(x, d, get.cpue(ar="5ABCD"))
         }else{
           write.data.file(x, d, get.cpue(ar="3CD"))
         }
       })
