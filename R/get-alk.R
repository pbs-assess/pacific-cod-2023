#Make age-length key (ALK) using age sample and functions in FSA package
get_ALK <- function(d,bin=2){
  #Round lengths, get sample size and smallest length in the sample
  d$length <- round(d$length,0)
  asamp <- nrow(d)
  psmall <- min(d$length, na.rm=T)

  #Sort samples by age then make ALK
  # can't pipe first step because lencat first argument is not data
  # possibly re-write this function
  d <- d[order(d$age),]
   d1 <- FSA::lencat(~length, data=d,startcat=psmall,w=bin)
   alk.raw <- table(d1$LCat, d1$age)
  # #Convert the raw age length key into proportions to get age-length key
   pcod.key <- prop.table(alk.raw,margin=1)
   pcod.key <- round(pcod.key,3)
   pcod.key
}
