#### OpenBugs Script Commands
ScriptCommands <- function(hateWindows = TRUE) {
  commands <- c("CHECK", "DATA", "COMPILE", "INITS",
                "GENINITS", "BEG", "UPDATE",
                "SET", "DICSET",
                "STATS", "DICSTATS", "CODA", "SAVE",
                "SETRN", "GETRN",
                "QUIT", "LBR")
  openBugs <- list("modelCheck", "modelData","modelCompile","modelInits",
                   "modelGenInits", "samplesBeg", "modelUpdate",
                   "samplesSet","dicSet",
                   "samplesStats", "dicStats", "samplesCoda", "modelSaveLog",
                   "modelSetRN", "modelGetRN",
                   "modelQuit", "\n")
  winBugs <- list("check", "data", "compile", "inits",
                  "gen.inits", "beg", "update",
                  "set", "dic.set",
                  "stats", "dic.stats", "coda", "save",
                  "set.seed", "get.seed",
                  "quit", "\n")
  comm <- if(hateWindows) openBugs else winBugs
  names(comm) <- commands
  comm
}

genBugsScript <-
  function(paramSet,
           n.chains,
           n.iter,
           n.burnin,
           n.thin,
           dic,
           model.file,
           data.file,
           inits.files,
           workingDir=NULL, #getwd(),
           bugsWorkingDir=getwd(), ## needs to be readable for BUGS
           script, #output
           debug=FALSE,
           useWine=FALSE,
           linbugs=TRUE, seed=314159) {
  if (n.chains != length(inits.files)) stop("length(inits.files) should equal n.chains.")
  ## n.iter <- n.burnin + n.thin * n.keep

  ## add deviance to the paramSet list
  paramSet <- c(paramSet, "deviance")

  ## setup workingDir
  if (is.null(workingDir)) {
    if (useWine) workingDir <- driveTr(bugsWorkingDir, .DriveTable)
    else workingDir <- bugsWorkingDir
  }
  if (linbugs) useWine <- FALSE
  ## necessary if useWine == TRUE
  if (useWine) {
    model.file <- sub(workingDir, bugsWorkingDir, model.file)
    data.file <- sub(workingDir, bugsWorkingDir, data.file)
    for (i in 1:length(inits.files))
      inits.files[i] <- sub(workingDir, bugsWorkingDir, inits.files[i])
  }

  ## attach the command list
  comm <- ScriptCommands(linbugs)
  attach(comm)
  on.exit(detach(comm))
  
  ## setup some file names
  coda  <- file.path(bugsWorkingDir, "coda")
  ## logodc <- file.path(bugsWorkingDir, "log.odc")
  logfile <- file.path(bugsWorkingDir, "log.txt")
  ## note that the order or arguments to INITS are different
  ## in WinBUGS and OpenBUGS
  initlist <- if (linbugs) paste(INITS, "(", "'", inits.files, "', ", 1:n.chains, ")", LBR, sep="") else paste(INITS, "(", 1:n.chains, ", '", inits.files, "')", LBR, sep="")
  savelist <- paste(SET, "(", paramSet, ")", LBR, sep="")
  ## write out to script.txt
  nburn <- ceiling(n.burnin / n.thin)
  nsamp <- ceiling((n.iter - n.burnin) / n.thin)
  cat (
       ##"display ('log')\n",
       CHECK, "('", model.file, "')", LBR,
       DATA, "('", data.file, "')", LBR,
       COMPILE, "(", n.chains, ")", LBR,
       initlist,
       GENINITS, "()", LBR,
       BEG, "(", nburn + 1, ")", LBR,
       SETRN, "(", seed, ")", LBR,
       UPDATE, "(", nburn, ", ", n.thin, ")", LBR,
       savelist,
       if (dic) c(DICSET, "()", LBR),
       UPDATE, "(", nsamp, ", ", n.thin, ")", LBR,
       STATS, "('*')", LBR,
       if (dic) c(DICSTATS, "(*)", LBR),
       CODA, "('*', '", coda, "')", LBR,
       ## "save ('", logodc, "')\n", 
       SAVE, "('", logfile, "')", LBR,
       if (linbugs) c(QUIT, "()", LBR),
       file=script, sep="", append=FALSE)
  if (!debug) cat (QUIT, "()", LBR, sep="", file=script, append=TRUE)
}
