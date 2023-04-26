build.doc <- function(knit.only = FALSE,
                      make.pdf  = TRUE,
                      doc.name  = "hake-assessment"){
  ## Use this function to build to doc entirely from within R
  ## Make sure you have created the .RData files by sourcing all.r
  ##  with the create.rdata.file variables set to TRUE.
  ## Once you have done that and run this function once within an R session,
  ##  you can go into the first knitr code chunk in hake-assessment.rnw and
  ##  set the call to load.models.into.parent.env() to FALSE,
  ##  which will save time for doing the build.
  ##
  ## knit.only - Only run knitr, not latex
  ## make.pdf - TRUE to make the pdf, if FALSE it will only go as far as
  ##  postscript.

  knit(paste0(doc.name,".rnw"))
  if(!knit.only){
    system(paste0("latex -synctex=1 ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("bibtex ", doc.name),
           invisible = FALSE,
           show.output.on.console = TRUE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("dvips ", doc.name,".dvi"),
           invisible = FALSE,
           show.output.on.console = TRUE)
    if(make.pdf){
      shell(paste0("ps2pdf ", doc.name, ".ps"))
    }
  }
}

f <- function(x, dec.points = 0, french = FALSE) {
  ## Format x to have supplied number of decimal points
  ## Make thousands seperated by commas and the number of decimal points given by
  ##  dec.points
  format(round(x,dec.points),
    big.mark = if (!french) "," else " ",
    decimal.mark = if (!french) "." else ",",
    nsmall = dec.points)
}

## -----------------------------------------------------------------------------
## Functions to make table generation easier

## Latex newline
latex.nline <- " \\\\ "

## Horizontal line
latex.hline <- " \\hline "

latex.perc <- function(vec){
  ## Return the input vector with all instances of % escaped properly for
  ##  xtable code - \\\\
  gsub("%", "\\\\%", vec)
}

latex.amp <- function(n = 1){
  ## Returns a string with n ampersands seperated by spaces. The string will
  ##  have one leading and one trailing space.
  paste0(rep(" &", n), " ", collapse = "")
}

latex.paste <- function(vec){
  ## Returns a string comprised of each element in the vector vec with an
  ##  ampersand in between. The string will have one leading and one
  ##  trailing space.
  paste(" ", vec, " ", collapse = " & ")
}

latex.bold <- function(txt){
  ## Returns the given text with the latex \\textbf{} macro around it
  paste0("\\textbf{", txt, "}")
}

latex.math.bold <- function(txt){
  ## Returns the given text with the latex \\mathbf{} macro and the
  ##  dollar signs around it
  paste0("$\\mathbf{", txt, "}$")
}

latex.italics <- function(txt){
  ## Returns the given text with the latex \\emph{} macro around it
  paste0("\\emph{", txt, "}")
}

latex.under <- function(txt){
  ## Returns the given text with the latex \\underline{} macro around it
  paste0("\\underline{", txt, "}")
}

latex.mlc <- function(latex.vec, make.bold = TRUE, math.bold = FALSE){
  ## Returns a string which has been glued together using multi-line-cell
  ##  macro for latex. If make.bold is TRUE, the \textbf macro will be
  ##  inserted unless math.bold is TRUE, then \\mathbf macro will be used
  if(make.bold){
    if(math.bold){
      latex.vec <- sapply(latex.vec, latex.math.bold)
    }else{
      latex.vec <- sapply(latex.vec, latex.bold)
    }
  }
  latex.str <- paste(latex.vec, collapse = latex.nline)
  paste0("\\mlc{", latex.str, "}")
}

latex.mcol <- function(ncol, just, txt){
  ## Returns the given text with the latex \\multicolumn{} macro around it
  ## ncol - the number of columns
  ## just - justification, e.g. "l", "c", or "r" for left, center, right
  paste0("\\multicolumn{", ncol, "}{", just, "}{", txt, "}")
}

latex.mrow <- function(nrow, just, txt, option = NULL){
  ## Returns the given text with the latex \\multicolumn{} macro around it
  ## nrow - the number of rows
  ## just - justification, e.g. "l", "c", or "r" for left, center, right
  ## option - optional argument for multirow
  if(is.null(option)){
    paste0("\\multirow{", nrow, "}{", just, "}{", txt, "}")
  }else{
    paste0("\\multirow{", nrow, "}{", just, "}[", option, "]{", txt, "}")
  }
}

latex.size.str <- function(fnt.size, spc.size){
  ## Returns a string which has the given font size and space size applied
  paste0("\\fontsize{", fnt.size, "}{", spc.size, "}\\selectfont")
}

latex.cline <- function(cols){
  ## Draw a horizontal line across the columns specified
  ## cols - a string in this format: "1-3" which means
  ##  the line should go across columns 1 to 3.
  paste0("\\cline{", cols, "}")
}

latex.cmidr <- function(cols, trim = "r"){
  ## Draw a horizontal line across the columns specified
  ## cols - a string in this format: "1-3" which means
  ##  the line should go across columns 1 to 3.
  ## trim - can be l, r, or lr and tells it to trim the
  ##  line a bit so that if there are two lines they don't
  ##  touch in the middle. (See booktabs package)
  paste0("\\cmidrule(", trim, "){", cols, "}")
}

latex.subscr <- function(main.txt, subscr.txt){
  ## Returns a latex string with main.txt subscripted by subscr.txt
  paste0(main.txt, "\\subscr{", subscr.txt, "}")
}

latex.subscr.ital <- function(main.txt, subscr.txt){
  ## Returns a latex string with main.txt subscripted by subscr.txt
  ##  where only main.txt is italicised
  paste0("\\emph{", main.txt, "}\\subscr{", subscr.txt, "}")
}

latex.supscr <- function(main.txt, supscr.txt){
  ## Returns a latex string with main.txt superscripted by supscr.txt
  paste0(main.txt, "\\supscr{", supscr.txt, "}")
}

## -----------------------------------------------------------------------------

get.rows.cols <- function(num){
  ## Returns a vector of length 2 representing the number of rows and columns
  ##  to use to pack a plot in a grid.
  if(num <= 64 && num > 49){
    if(num <= 56){
      nside <- c(8,7)
    }else{
      nside <- c(8,8)
    }
  }else if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}

get.quants <- function(data,
                       probs){
  ## Return the column quantiles for data matrix.
  ## The median along with the confidence interval 'ci'
  ## will be calculated and the quantiles returned.
  if(is.null(dim(data))){
    ## It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    ## It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  quants
}

install.packages.if.needed <- function(package.name){
  if(!(package.name %in% rownames(installed.packages()))){
    install.packages(package.name)
  }
}

split.prior.info <- function(prior.str,
                             dec.points = 1,
                             first.to.lower = FALSE){
  ## Get priors information from prior.str which is a string like
  ## Lognormal(2.0,1.01)
  ## Returns a vector of length 3:
  ## "Lognormal", 2.0, 1.01
  ## if first.to.lower = TRUE, makes the first letter of the name of the prior lower case.
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- f(as.numeric(p[1]), dec.points)
  p.sd <- f(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}

cohortCatch <- function(cohort, catage, ages = 0:20) {
  cohort.yrs <- cohort + ages
  caa <- as.matrix(catage[catage$Yr %in% cohort.yrs, as.character(ages)])
  w <- base.model$wtatage
  w$yr <- w$yr * -1
  waa <- w[w$fleet == 1 & w$yr %in% cohort.yrs, ]
  waa <- waa[, names(waa) %in% ages]
  catch.waa <- as.matrix(caa * waa)

  ind <- 1:(nrow(caa) + 1)
  if(length(ind) > length(ages)){
    ind <- 1:nrow(caa)
  }
  cohort.catch <- diag(catch.waa[,ind])
  names(cohort.catch) <- cohort.yrs[1:(nrow(caa))]
  return(cohort.catch)
}

get.age.prop <- function(vec, place = 1){
  ## returns the age prop and the age itself for the place,
  ## where place is 1=max, 2-second highest, etc.
  prop <- rev(sort(vec))
  prop <- prop[place]
  age <- as.numeric(names(vec[vec == prop]))
  return(c(age, prop))
}

get.shade <- function(color, opacity){
  # If color is a single R color string or single number,
  #  returns an rgb string of the specified color and opacity
  # If color is a vector of cR color strings or numbers,
  #  returns a vector of rgb strings of the specified color and opacity.
  # If the opacity argument is non-integer or not between 0 and 99, NULL will be returned.
  # - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
  # Notes: format of returned string is #RRGGBBAA
  #        where RR=red, a 2-hexadecimal-digit string
  #        GG=green, a 2-hexadecimal-digit string
  #        BB=blue, a 2-hexadecimal-digit string
  #        AA=alpha or opacity
  #
  # The opacity agrument is scalar and will be applied to all colors.
  if(!(opacity %% 1 == 0) || opacity<0 || opacity>99){
    cat0(.PROJECT_NAME,"->",currFuncName,"opacity argument must be an integer between 0 and 99.")
    return(NULL)
  }
  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow=3,ncol=ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",colorHEX[1,col],colorHEX[2,col],colorHEX[3,col],opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i])==1){
        colorHEX[i] <- paste0("0",colorHEX[i])
      }
    }
    shade <- paste0("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity)
  }
  return(shade)
}

remove.all.objects.except <- function(vars){
  # Removes every object in the workspace except for what is in the vars list.
  # Upon finishing, the workspace will contain whatever is in the vars list,
  #  plus the object 'remove.all.objects.except' (this function)

  vars <- c(vars, "remove.all.objects.except")
  keep <- match(x = vars, table = ls(all = TRUE, envir = .GlobalEnv))
  if(!any(is.na(keep))){
    rm(list=ls(all = TRUE, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
  }
}

pad.num <- function(num, digits = 0){
  ## Takes an integer, num and turns it into a string
  ## If the string is less than digits long, it will
  ## be prepended with zeroes
  if(digits < 1) stop("Error in pad.num - digits must be positive\n")
  sapply(num, function(x){ paste0(rep("0", digits - nchar(as.character(x))), as.character(x))})
}

t.pn <- function(){
  ## test pad.num
  cat("pad.num(0, 1) = ", pad.num(0, 1), "\n")
  cat("pad.num(1, 2) = ", pad.num(1, 2), "\n")
  cat("pad.num(10, 2) = ", pad.num(10, 2), "\n")
  cat("pad.num(10, 3) = ", pad.num(10, 3), "\n")
  cat("pad.num(10, 0) = ", pad.num(10, 0), "\n")
}

print.model.message <- function(model.dir.names, model.names, group, model.type){
  ## Print out a message stating the model directory names and pretty names,
  ##  for the group number given. If bridge is TRUE, it is a bridge model group,
  ##  if bridge is FALSE, it is a sensitivity model group.

  cat0("***")
  cat0(model.type, " model group ", group, " directories: ")
  cat(paste0("  ", model.dir.names), sep = "\n")
  cat0(model.type, " model group ", group, " pretty names: ")
  cat(paste0("  ", model.names), sep = "\n")
  cat0("***")
}

curr.fn.finder <- function(skipframes = 0,
                           skipnames = "(FUN)|(.+apply)|(replicate)",
                           ret.if.none = "Not in function",
                           ret.stack = FALSE,
                           extra.perf.per.level = "\t"){
  ## Get the current function name from within the function itself.
  ## Used to prepend the function name to all messages so that the
  ## user knows where the message came from.
  prefix <- sapply(3 + skipframes + 1:sys.nframe(), function(i){
    currv <- sys.call(sys.parent(n = i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    return(ret.if.none)
  }else if(ret.stack){
    return(paste(rev(prefix), collapse = "|"))
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra.perf.per.level, length(prefix) - 1), collapse = ""), retval)
    }
    return(retval)
  }
}

get.curr.func.name <- function(){
  ## Returns the calling function's name followed by ": "
  func.name <- curr.fn.finder(skipframes = 1) # skipframes=1 is there to avoid returning getCurrFunc itself
  ## Strip extraneous whitespace
  func.name <- gsub("\t+", "", func.name)
  func.name <- gsub("\ +", "", func.name)
  func.name <- paste0(func.name,": ")
  return(func.name)
}

cat0 <- function(...){
  ## Wrapper function to make cat have no space and insert a newline at the end.
  ## Inspired by the paste0 function.
  cat(..., "\n", sep = "")
}

number.to.word <- function(x, th = FALSE, cap.first = FALSE){
  ## https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  ## if th is TRUE, the th version will be returned, e.g. 4 = fourth
  ## if cap.first is TRUE, the first letter will be capitalized
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if(nDigits == 1) as.vector(ones[digits])
    else if(nDigits == 2)
      if(x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    ## Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    ## Clear any trailing " and"
    text=gsub(" and$","",text)
    ##Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  ## Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  j <- helper(x)
  ## Cgrandin added the 'th' bit
  if(th){
    j <- strsplit(j, " ")[[1]]
    first <- j[-length(j)]
    last <- j[length(j)]
    if(last == "one"){
      last <- "first"
    }else if(last == "two"){
      last <- "second"
    }else if(last == "three"){
      last <- "third"
    }else if(last == "five"){
      last <- "fifth"
    }else if(last == "eight"){
      last <- "eighth"
    }else if(last == "nine"){
      last <- "ninth"
    }else if(last == "twelve"){
      last <- "twelfth"
    }else if(last == "twenty"){
      last <- "twentieth"
    }else if(last == "thirty"){
      last <- "thirtieth"
    }else if(last == "forty"){
      last <- "fortieth"
    }else if(last == "fifty"){
      last <- "fiftieth"
    }else if(last == "sixty"){
      last <- "sixtieth"
    }else if(last == "seventy"){
      last <- "seventieth"
    }else if(last == "eighty"){
      last <- "eightieth"
    }else if(last == "ninety"){
      last <- "ninetieth"
    }else{
      last <- paste0(last, "th")
    }
    j <- paste(c(first, last), collapse = " ")
  }
  if(cap.first){
    j <- paste0(toupper(substr(j, 1, 1)), substr(j, 2, nchar(j)))
  }
  return(j)
}

## *****************************************************************************
## The following three functions give the ability to assign more than one variable at once.
## Example Call;  Note the use of set.elems()  AND  `%=%`
## Right-hand side can be a list or vector
## set.elems(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))
## set.elems(d, e, f) %=%  101:103
## # Results:
## > a
## [1] "hello"
## > b
## [1] 123
## > c
## [[1]]
## [1] "apples, oranges"
## > d
## [1] 101
## > e
## [1] 102
## > f
## [1] 103

## Generic form
"%=%" <- function(l, r, ...) UseMethod("%=%")

## Binary Operator
"%=%.lhs" <- function(l, r, ...) {
  env <- as.environment(-1)
  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extend.to.match(r, l)
  }
  for(II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir = env)
  }
}

## Used if LHS is larger than RHS
extend.to.match <- function(src, destin) {
  s <- length(src)
  d <- length(destin)
  # Assume that destin is a length when it is a single number and src is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  dif <- d - s
  if (dif > 0) {
    src <- rep(src, ceiling(d/s))[1:d]
  }
  return (src)
}

set.elems <- function(...) {
  list.tmp <-  as.list(substitute(list(...)))[-1L]
  class(list.tmp) <-  "lhs"
  return(list.tmp)
}

cbind.fill <- function(...){
  ## equivalent of cbind(df, xx) where df is an empty data frame.
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

strip.columns <- function(vec, names){
  ## Return a vector which is the same as the vector 'vec'
  ## but with the matching col.names removed
  return(vec[!names(vec) %in% names])
}

get.align <- function(num,
                      first.left = TRUE, ## Keep the first column left-justified
                                         ## If FALSE, it will be justified according to the 'just' argument
                      just = "r"         ## just is the justification to use for the columns, "r", "l", or "c"
                      ){
  ## Returns a character vector used in the align argument of the xtable command.
  ## e.g. posterior output tables, reference point tables. Most tables really.
  ## num is the number of columns in the table
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  return(align)
}

rc <- rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

plotBars.fn <- function(x,y,gap=0,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value,...)
  if(add) points(x,y$value,...)
  segments(x,y$lo,x,y$value-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi,x,y$value+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

plotBars.fn <- function(x,y,gap=0,scalar=1e6,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value/scalar,...)
  if(add) points(x,y$value/scalar,...)
  segments(x,y$lo/scalar,x,y$value/scalar-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi/scalar,x,y$value/scalar+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

panel.letter <- function(letter){
  ## Adds letters to plot panels
  ## letter - the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05 * (usr[2] - usr[1])
  inset.y <- 0.05 * (usr[4] - usr[3])
  if(is.character(letter)){
    let <- letter
  }else{
    let <- letters[letter]
  }
  text(usr[1] + inset.x,
       usr[4] - inset.y,
       paste0("(", let, ")"),
       cex = 1.,
       font = 1)
}

addpoly <- function(yrvec, lower, upper, color = 1, shade.col = NULL){
  lower[lower<0] <- 0 ## max of value or 0
  if(is.null(shade.col)){
    shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)
  }
  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
}

curfnfinder <- function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
    retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  # Get the current function name from within the function itself.
  # Used to prepend the function name to all messages so that the
  # user knows where the message came from.
    prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
            currv<-sys.call(sys.parent(n=i))[[1]]
            return(currv)
        })
    prefix[grep(skipnames, prefix)] <- NULL
    prefix<-gsub("function \\(.*", "do.call", prefix)
    if(length(prefix)==0)
    {
        return(retIfNone)
    }
    else if(retStack)
    {
        return(paste(rev(prefix), collapse = "|"))
    }
    else
    {
        retval<-as.character(unlist(prefix[1]))
        if(length(prefix) > 1)
        {
            retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
        }
        return(retval)
    }
}

catw <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE, prefix=0)
{
  # writes out some innformation on the calling function to screen
    if(is.numeric(prefix))
    {
        prefix<-curfnfinder(skipframes=prefix+1) #note: the +1 is there to avoid returning catw itself
        prefix<-paste(prefix, ": ", sep="")
    }
    cat(prefix, ..., format(Sys.time(), "(%Y-%m-%d %H:%M:%S)"), "\n",
        file = file, sep = sep, fill = fill, labels = labels, append = append)
}

panel.letter <- function(letter){
  ## Adds letters to plot panels
  ## letter - the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05 * (usr[2] - usr[1])
  inset.y <- 0.05 * (usr[4] - usr[3])
  if(is.character(letter)){
    let <- letter
  }else{
    let <- letters[letter]
  }
  text(usr[1] + inset.x,
       usr[4] - inset.y,
       paste0("(", let, ")"),
       cex = 1.,
       font = 1)
}

get.latex.name <- function(name, addToQ = 0){
  ## Return a pretty version of the parameter name found in variable 'name'
  ##
  ## addToQ - an integer to the parameter name for the q's. This is necessary
  ##  because iscam sets the q parameter names to 1, 2, 3... regardless of the
  ##  gear number. i.e. if gear 1 is a trawl fishery and gear 2 is a survey,
  ##  iscam will call q1 the survey gear. We must add 1 to it to get q2 to
  ##  accurately portray the survey gear number
  if(name == "ro") return(expression("R"[0]))
  if(name == "rbar") return(expression(bar("R")))
  if(name == "rinit") return(expression(bar("R")[init]))
  if(name == "m") return(expression("M"))
  if(name == "bo") return(expression("B"[0]))
  if(name == "vartheta") return(expression(vartheta))
  if(name == "rho") return(expression(rho))
  if(name == "bmsy") return(expression("B"[MSY]))
  if(name == "msy") return(expression("MSY"))
  if(name == "fmsy") return(expression("F"[MSY]))
  if(name == "umsy") return(expression("U"[MSY]))
  if(name == "ssb") return(expression("SSB"))
  if(name == "sel1") return(expression(hat(a)[1]))
  if(name == "selsd1") return(expression(hat(gamma)[1]))
  if(name == "sel2") return(expression(hat(a)[2]))
  if(name == "selsd2") return(expression(hat(gamma)[2]))
  if(name == "sel3") return(expression(hat(a)[3]))
  if(name == "selsd3") return(expression(hat(gamma)[3]))
  if(name == "sel4") return(expression(hat(a)[4]))
  if(name == "selsd4") return(expression(hat(gamma)[4]))
  if(name == "sel5") return(expression(hat(a)[5]))
  if(name == "selsd5") return(expression(hat(gamma)[5]))

  if(name == "log_ro") return(expression("ln(R"[0]*")"))
  if(name == "h") return(expression("h"))
  if(name == "m1") return(expression("M"[1]))
  if(name == "m2") return(expression("M"[2]))
  if(name == "log_m") return(expression("ln(M)"))
  if(name == "log_rbar") return(expression("ln("*bar("R")*")"))
  if(name == "log_rinit") return(expression("ln("*bar("R")[init]*")"))

  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(substitute("q"[digit], list(digit = digit)))
  }

  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(substitute("ln(q"[digit]*")", list(digit = digit)))
  }

  NULL
}

get.rmd.name <- function(name){

  if(name == "ro") return("$R_0$")
  if(name == "m") return("$M$")
  if(name == "bo") return("$B_0$")
  if(name == "log_ro") return("$ln(R_0)$")
  if(name == "h") return("$h$")
  if(name == "log_m") return("$ln(M)$")
  if(name == "log_avgrec") return("$ln(\\bar{R})$")
  if(name == "log_recinit") return("$ln(R_{init})$")
  if(name == "log_avgrec") return("$ln(\\bar{R})$")
  if(name == "rho") return("$\\rho$")
  if(name == "kappa") return("$\\kappa$")

  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(paste0("$q_", digit, "$"))
  }

  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(paste0("$ln(q_", digit, ")$"))
  }

  NULL
}

draw.envelope <- function(yrs,
                          quants,
                          col = "black",
                          first,
                          opacity = 75,
                          ...){
  ## Draw a time series envelope on a device on which plot.new has already
  ##  been called.
  ## Assumptions: quants is a 3-row matrix, where the middle row is the
  ##  median and the other two are the lower and upper values for some
  ##  confidence interval.
  ## first - boolean, if TRUE, plot will be called. If FALSE, lines will be
  ##  called.
  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs,
         median,
         type = "l",
         col = col,
         lty = 1,
         lwd = 2,
         ...)
    shade <- get.shade(col, opacity)
    poly.yrs <- c(yrs, rev(yrs))
    poly.ci    <- c(lower, rev(upper))
    polygon(poly.yrs, poly.ci, col = shade)
  }else{
    lines(yrs,
          median,
          type = "l",
          col = col,
          lty = 1,
          lwd = 2,
          ...)
    ## Upper and lower part of CI
    lines(yrs,
          lower,
          col = col,
          lty = 5,
          lwd = 1)
    lines(yrs,
          upper,
          col = col,
          lty = 5,
          lwd = 1)
  }
}

c.model.list <- function(..., recursive = FALSE){
  ## Extract the model class objects from the list of model lists,
  ##  and merge them into a single model list containing all the model
  ##  class objects.
  ## To use: c(model.list.1, model.list.2)

  lst <- list(...)
  ret.lst <- NULL
  ind <- 1
  for(i in 1:length(lst)){
    if(class(lst[[i]]) != model.lst.class){
      stop("List element ", i, " is not of the class '", model.lst.class, "'.")
    }
    for(j in 1:length(lst[[i]])){
      if(class(lst[[i]][[j]]) != model.class){
        stop("Sublist element ", j, " of list element ", i,
             " is not of the class '", model.class, "'.")
      }
      ret.lst[[ind]] <- lst[[i]][[j]]
      ind <- ind + 1
    }
  }
  class(ret.lst) <- model.lst.class
  ret.lst
}

and.string <- function(vec){
  ## Make a string out of the strings in vec, by glueing together with commas
  ## and placing 'and' before the last one

  if(length(vec) == 1){
    return(vec)
  }
  if(length(vec) == 2){
    return(paste0(vec[1], " and ", vec[2]))
  }

  j <- paste(vec[-length(vec)], collapse = ", ")
  paste(j, "and", vec[length(vec)])
}

avg.models <- function(models,
                       probs = c(0.025, 0.5, 0.975)){
  # Average the models by merging their posteriors
  # Return a model object

  avg.model <- models[[1]]

  ## Biomass
  bt <- lapply(models,
               function(x){
                 x$mcmccalcs$sbt.dat
               })
  bt <- bind_rows(bt)
  mpd.bt <- lapply(models,
               function(x){
                 as.data.frame(x$mpd$sbt)
               })
  names(mpd.bt) <- seq(1, length(mpd.bt))
  mpd.bt <- apply(t(bind_cols(mpd.bt)), 2, mean)

  avg.model$mcmccalcs$sbt.dat <- bt
  avg.model$mcmccalcs$sbt.quants <- rbind(apply(bt,
                                                2,
                                                quantile,
                                                probs = probs),
                                          mpd.bt)
  rownames(avg.model$mcmccalcs$sbt.quants)[4] <- "MPD"

  ## B0
  r.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$r.quants
                     })
  bo.raw <- lapply(r.quants,
                   function(x){
                     x[rownames(x) == "bo", ]})
  bo <- lapply(bo.raw,
               function(x){
                 as.numeric(x[,2:4])})
  bo <- as.data.frame(do.call(rbind, bo))
  avg.model$mcmccalcs$r.quants[1, 2:4] <- apply(bo, 2, mean)

  ## Recuitment
  recr <- lapply(models,
                 function(x){
                   x$mcmccalcs$recr.dat
                 })
  recr <- bind_rows(recr)

  len.of.recr.mpd <-  sapply(models,
                             function(x){
                               length(x$mpd$rt)
                             })

  mpd.recr <- lapply(seq_along(models),
                     function(x){
                       j <- models[[x]]$mpd$rt
                       if(len.of.recr.mpd[x] < max(len.of.recr.mpd)){
                         j <- c(rep(NA,
                                    max(len.of.recr.mpd) - len.of.recr.mpd[x]),
                                j)
                       }
                       as.data.frame(j)
                     })
  names(mpd.recr) <- seq(1, length(mpd.recr))
  mpd.recr <- apply(t(bind_cols(mpd.recr)), 2, mean)

  avg.model$mcmccalcs$recr.dat <- recr
  avg.model$mcmccalcs$recr.quants <- rbind(apply(recr,
                                                 2,
                                                 quantile,
                                                 probs = probs,
                                                 na.rm = TRUE),
                                           mpd.recr)
  rownames(avg.model$mcmccalcs$recr.quants)[4] <- "MPD"

  ## Projections
  ## Assume all models have the same TACs
  tac <- models[[1]]$proj$tac.vec
  proj.all <- NULL
  for(t in seq_along(tac)){
    proj <- lapply(models,
                   function(x){
                         filter(x$mcmccalcs$proj.dat,
                                TAC == tac[t])
                   })
    proj.all <- rbind(proj.all, bind_rows(proj))
  }
  avg.model$mcmccalcs$proj.dat <- proj.all
  avg.model <- list(avg.model)
  class(avg.model) <- model.lst.class

  avg.model
}
