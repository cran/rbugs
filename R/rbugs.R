#### configurations  12/15/2003
## BUGS stores the executable of bugs
## workingDir is the directory to save all files, default is tempdir()
## bugsWorkingDir is the directory for wine to use windows type directory
## WINE stores the executable of wine
## useWine = TRUE if use wine

## example usage on linux
## schools.sim <- rbugs(data=schools.data, inits, parameters, "schools.bug", n.chains=3, n.iter=1000, workingDir="/var/tmp/jyan/c/tmp", bugsWorkingDir="c:/tmp", useWine=T, wine="/var/scratch/jyan//wine/wine-20031016/wine", debug=T)


rbugs <- function(data, inits, paramSet, model,
                  n.chains=1, n.iter=2000, n.burnin=floor(n.iter/2),
                  n.thin=max(1, floor(n.chains*(n.iter-n.burnin)/1000)),
                  debug=FALSE,
                  bugs=Sys.getenv("BUGS"),
                  ##"c:/Program Files/WinBUGS14/WinBUGS14.exe",
                  workingDir = getwd(),
                  ##"/var/scratch/jyan/c/tmp", # native
                  bugsWorkingDir = workingDir,
                  ##"c:/tmp",
                  useWine = FALSE, 
                  wine = Sys.getenv("WINE")
                  ## "/var/scratch/jyan/wine/wine-20031016/wine"
                  ){
  ##  start.time <- Sys.time ()
  os.type <- .Platform$OS.type
  if (os.type == "windows") {
    if (!file.exists(bugs))
      stop(paste("BUGS executable", bugs, "does not exists."))
  }
  else if (os.type == "unix") {
    if (!useWine) stop ("Please set useWine = TRUE.")
    if (!file.exists(wine))
      stop(paste("wine executable", wine, "does not exists."))
    ## how to check the existence of WinBUGS???
  }
  else warning("This function has not been tested on mac-os.")
  ## prepare the model file by 
  ## making a copy of model to the working directory
  model.file <- paste(workingDir, "model.txt", sep="/")
  file.copy(model, model.file, overwrite=TRUE)
  ## prepare the data file
  data.file <- paste(workingDir, "data.txt", sep="/")
  genDataFile(data, data.file)
  ## prepare the inits files
  inits.file.stem <- paste(workingDir, "init", sep="/")
  genInitsFile(n.chains, inits, inits.file.stem)
  inits.files <- paste(inits.file.stem, 1:n.chains, ".txt", sep="")
  ## prepare the script file
  script.file <- paste(workingDir, "script.txt", sep="/")
  genBugsScript(paramSet, n.chains, n.iter, n.burnin, n.thin,
                model.file, data.file, inits.files,
                workingDir, bugsWorkingDir,
                script.file, debug, useWine)
  ## run bugs
  if (useWine) script.file <- gsub(workingDir, bugsWorkingDir, script.file)
  runBugs(bugs, script.file, useWine, wine)
  ## collect the output
  out <- getBugsOutput(n.chains, workingDir)
  out
}


genDataFile <- function(dataList, dataFile) {
  if (is.numeric(unlist(dataList))) {
    ## cat(dput2bugs(dataList), file = data.file)
    cat(format4Bugs(dataList), file = dataFile, fill = TRUE)
  }
  else {
    data <- lapply(dataList, get, pos = 1)
    names(data) <- dataList
    ## cat(dput2bugs(data), file = data.file, fill = TRUE)
    cat(format4Bugs(data), file = dataFile, fill = TRUE)
  }
}


genInitsFile <- function(n.chains, inits, initsFileStem) {
  for (i in 1:n.chains) {
    file <- paste(initsFileStem, i, ".txt", sep="")
    if (is.function(inits)) cat(format4Bugs(inits()), file=file, fill = TRUE)
    else cat(format4Bugs(inits[[i]]), file=file, fill = TRUE)
  }
}


genBugsScript <- function(paramSet,
                          n.chains,
                          n.iter,
                          n.burnin,
                          n.thin,
                          model.file,
                          data.file,
                          inits.files,
                          workingDir=getwd(),
                          ## needs to be readable for BUGS
                          bugsWorkingDir=workingDir, 
                          script, #output
                          debug=FALSE, useWine=FALSE) {
  if (n.chains != length(inits.files)) stop("length(inits.files) should equal n.chains.")
  ## n.iter <- n.burnin + n.thin * n.keep

  ## add deviance to the paramSet list
  paramSet <- c(paramSet, "deviance")

  
  ## necessary if useWine == TRUE:
  if (useWine) {
    model.file <- sub(workingDir, bugsWorkingDir, model.file)
    data.file <- sub(workingDir, bugsWorkingDir, data.file)
    for (i in 1:length(inits.files))
      inits.files[i] <- sub(workingDir, bugsWorkingDir, inits.files[i])
  }
  
  history <- paste (bugsWorkingDir, "history.txt", sep="/")
  coda  <- paste (bugsWorkingDir, "coda", sep="/")
  logodc <- paste(bugsWorkingDir, "log.odc", sep="/")
  logfile <- paste(bugsWorkingDir, "log.txt", sep="/")
  initlist <- paste ("inits (", 1:n.chains, ", '", inits.files, "')\n", sep="")
  savelist <- paste ("set (", paramSet, ")\n", sep="")
  ## write out to script.txt
  cat (
       "display ('log')\n",
       "check ('", model.file, "')\n",
       "data ('", data.file, "')\n",
       "compile (", n.chains, ")\n",
       initlist,
       "gen.inits()\n",
       "beg (", ceiling(n.burnin/n.thin)+1, ")\n",
       "thin.updater (", n.thin, ")\n",
       savelist,
       "dic.set()\n",
       "update (", ceiling(n.iter/n.thin), ")\n",
       "stats (*)\n",
       "dic.stats()\n",
       "history (*, '", history, "')\n",
       "coda (*, '", coda, "')\n",
       "save ('", logodc, "')\n", 
       "save ('", logfile, "')\n", file=script, sep="", append=FALSE)
  if (!debug) cat ("quit ()\n", file=script, append=TRUE)
  sims.files <- paste ("coda", 1:n.chains, ".txt", sep="")
#  for (i in 1:n.chains)
#    cat ("Bugs did not run correctly.\n", file=sims.files[i], append=FALSE)
}



#### run bugs
runBugs <- function(bugs=Sys.getenv("BUGS"),
                    script,
                    useWine=FALSE,
                    wine = Sys.getenv("WINE")) {
#  BUGS <- Sys.getenv("BUGS")
#  if (!file.exists(BUGS)) stop(paste(BUGS, "does not exists."))
  if (is.na(pmatch("\"", bugs)))bugs <- paste("\"", bugs, "\"", sep="")
  if (is.na(pmatch("\"", script))) script <- paste("\"", script, "\"", sep="")
  command <- paste(bugs, "/par", script)
  if (useWine) {
    command <- paste(wine, command)

    ## put a "q" to quit from wine debugg
    q.tmp <- tempfile("q")
    on.exit(unlink(q.tmp))
    cat("q\n", file=q.tmp)
    command <- paste(command, "< ", q.tmp)

    ## redirect the erorr/warning message of Wine
    wine.warn <- tempfile("warn")
    on.exit(unlink(wine.warn))
    command <- paste(command, ">", wine.warn, " 2>&1 ")
  }
  
  ## execute it!
  system(command)
}


#### functions to get the output



getBugsOutput <- function(n.chains, workingDir) {
  coda  <- paste(workingDir, "coda", sep="/")
  codaFiles <- paste(coda, 1:n.chains, ".txt", sep="")
  codaIndexFile <- paste(coda, "Index.txt", sep="")
  codaIndex <- read.table(codaIndexFile, header=FALSE, sep="\t", as.is=TRUE)
  n.keep <- codaIndex[1, 3] - codaIndex[1, 2] + 1
  nodes <- codaIndex[, 1]
  n.param <- length(nodes)
  output <- list()
  for (i in 1:n.chains) {
    foo <- read.table(codaFiles[i], header=FALSE)
    iter <- foo[1:n.keep, 1]
    vals <- matrix(foo[,2], n.keep, n.param)
    dat <- as.data.frame(cbind(iter, vals))
    names(dat) <- c("iter", nodes)
    output[[i]] <- dat
  }    
  output
}
