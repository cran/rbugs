## Modified Function from EmBedBugs package (embedR), by Kenneth Rice
## adapted from http://www.mrc-bsu.cam.ac.uk/personal/ken/embed.html
## old page was: www.statslab.cam.ac.uk/~krice/embed.html
formatData <- function (datalist){
    if (!is.list(datalist) || is.data.frame(datalist)) 
        stop("Argument to format.data must be a list.")
    n <- length(datalist)
    datalist.string <- as.list(rep(NA, n))
    for (i in 1:n) {
        if (length(datalist[[i]]) == 
            1) 
            datalist.string[[i]] <- paste(names(datalist)[i], 
                "=", as.character(datalist[[i]]), sep = "")
        if (is.vector(datalist[[i]]) & length(datalist[[i]]) > 
            1) 
            datalist.string[[i]] <- paste(names(datalist)[i], 
                "=c(", paste(as.character(datalist[[i]]), collapse = ", "), 
                ")", sep = "")
        if (is.array(datalist[[i]])) 
            datalist.string[[i]] <- paste(names(datalist)[i], 
                "= structure(.Data= c(", paste(as.character(as.vector(aperm(datalist[[i]]))), 
                  collapse = ", "), "), .Dim=c(", paste(as.character(dim(datalist[[i]])), 
                  collapse = ", "), "))", sep = "")
    }
    datalist.tofile <- paste("list(", paste(unlist(datalist.string), 
        collapse = ", "), ")", sep = "")
    return(datalist.tofile)
}

format4Bugs <- function (dataList, digits=5){
  if (!is.list(dataList) || is.data.frame(dataList)) 
    stop("Argument to formatdata() must be a list.")
#   tmp <- tempfile("dat")
#   on.exit(unlink(tmp))
  ## make sure there is no more than 14 digits
  if (digits > 14) digits <- 14
  dataListString <- lapply(dataList,
                            function(x) {
                              if (is.data.frame(x)) x <- as.matrix(x)
                              dimnames(x) <- NULL
                              if (is.integer(x)) formatC(x, digits=0)
                              ## make sure "E" instead of "e"
                              else formatC(x, format="E", digits=digits)
                            })

  foo <- formatData(dataListString)
  foo <- gsub('"', '', foo)
  foo
# The following method does not always work!  
#   dput(dataListString, tmp)
#   foo <- readLines(tmp)
#   foo <- sub('^structure\\(', '', foo)
#   foo <- gsub('structure\\(c', 'structure\\(.Data=c', foo)
#   foo <- sub('\\), .Names.*$', '\\)', foo)
#   foo <- gsub('"', '', foo)
#   foo <- gsub('as.integer\\(c\\(', 'c\\(', foo)
#   foo <- gsub('\\)\\)', '\\)', foo)
#   foo
}


## generate t.cen from a Surv object

# t.cen <- function(time, status) {
#   ifelse(status == 1, 0, time)
# }


