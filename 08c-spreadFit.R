spreadFitPaths[["cachePath"]] <- file.path(cacheDir, "cache_spreadFit", runName)
do.call(setPaths, spreadFitPaths)

gid_spreadOut <- gdriveSims[studyArea == studyAreaName & simObject == "spreadOut" & runID == run, gid]
upload_spreadOut <- reupload | length(gid_spreadOut) == 0

## TODO: remove this workaround
fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]] <- as.data.table(fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]])

extremeVals <- 4
lowerParamsNonAnnual <- rep(-extremeVals, times = ncol(fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]]) - 1)
lowerParamsAnnual <- c(-extremeVals, -extremeVals)
upperParamsNonAnnual <- rep(extremeVals, times = length(lowerParamsNonAnnual))
upperParamsAnnual <- c(0, extremeVals) ## youngAge <= 0
lowerParams <- c(lowerParamsAnnual, lowerParamsNonAnnual)
upperParams <- c(upperParamsAnnual, upperParamsNonAnnual)

## Spread log function bounds

## for logistic3p
# lower <- c(0.22, 0.001, 0.001, lowerParams)
# upper <- c(0.29, 10, 10, upperParams)

lower <- c(0.25, 0.2, 0.1, lowerParams)
upper <- c(0.276, 2, 4, upperParams)
dfT <- cbind(c("lower", "upper"), t(data.frame(lower, upper)))
message("Upper and Lower parameter bounds are:")
Require:::messageDF(dfT)

cores <-  if (peutils::user("achubaty")) {
  if (Sys.info()[["nodename"]] == "picea.for-cast.ca") {
    if (fitUsing == 3) {
      c(rep("localhost", 25), rep("pinus.for-cast.ca", 8), rep("pseudotsuga.for-cast.ca", 67))
    } else if (fitUsing == 2) {
      c(rep("localhost", 68), rep("pinus.for-cast.ca", 32))
    } else if (fitUsing == 1) {
      rep("pseudotsuga.for-cast.ca", 100)
    }
  } else if (Sys.info()[["nodename"]] == "pseudotsuga.for-cast.ca") {
    rep("localhost", 100)
  } else if (grepl("spades", Sys.info()["nodename"])) {
    pemisc::makeIpsForNetworkCluster(ipStart = "10.20.0",
                                     ipEnd = c(106, 217, 213, 220),
                                     availableCores = c(15, 25, 40, 40),
                                     availableRAM = c(250, 250, 500, 500),
                                     localHostEndIp = localHostEndIp,
                                     proc = "cores",
                                     nProcess = length(lower),
                                     internalProcesses = 10,
                                     sizeGbEachProcess = 1)
  }
} else {
  min(100, rep("localhost", parallel::detectCores() / 2)) ## needed even if spreadFit not being run!
}

spreadFitParams <- list(
  fireSense_SpreadFit = list(
    cloudFolderID_DE = cloudCacheFolderID,
    cores = cores,
    DEoptimTests = if (peutils::user("emcintir")) "snll_fs" else c("adTest", "snll_fs"), # Can be one or both of c("adTest", "snll_fs")
    doObjFunAssertions = FALSE,
    iterDEoptim = if (peutils::user("emcintir")) 300 else 150,
    iterStep = if (peutils::user("emcintir")) 300 else 150,
    iterThresh = 396L,
    libPathDEoptim = libPathDEoptim,
    lower = lower,
    maxFireSpread = max(0.28, upper[1]),
    mode = c("fit", "visualize"), ## combo of "debug", "fit", "visualize"
    mutuallyExclusive = list("youngAge" = c("class", "nf_")),
    NP = length(cores),
    objFunCoresInternal = 1L,
    objfunFireReps = 100,
    # onlyLoadDEOptim = FALSE,
    rescaleAll = TRUE,
    trace = 1,
    SNLL_FS_thresh = NULL, # NULL means 'autocalibrate' to find suitable threshold value
    upper = upper,
    # urlDEOptimObject = if (peutils::user("emcintir")) "spreadOut_2021-02-11_Limit4_150_SNLL_FS_thresh_BQS16t" else NULL,
    useCloud_DE = useCloudCache,
    verbose = TRUE,
    visualizeDEoptim = FALSE,
    .plot = FALSE, # TRUE,
    .plotSize = list(height = 1600, width = 2000)
  )
)

#spreadOut <- readRDS("spreadOut_2021-03-01_Limit4_300_SNLL_FS_thresh_jF0YfA")
spreadFitObjects <- list(
  fireBufferedListDT = fSsimDataPrep[["fireBufferedListDT"]],
  firePolys = fSsimDataPrep[["firePolys"]],
  fireSense_annualSpreadFitCovariates = fSsimDataPrep[["fireSense_annualSpreadFitCovariates"]],
  fireSense_nonAnnualSpreadFitCovariates = fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]],
  fireSense_spreadFormula = fSsimDataPrep[["fireSense_spreadFormula"]],
  flammableRTM = fSsimDataPrep[["flammableRTM"]],
  rasterToMatch = fSsimDataPrep[["rasterToMatch"]],
  spreadFirePoints = fSsimDataPrep[["spreadFirePoints"]],
  studyArea = fSsimDataPrep[["studyArea"]]
)

fspreadOut <- simFile(paste0("spreadOut_", studyAreaName, "_", run), Paths$outputPath, ext = simFileFormat)
if (isTRUE(usePrerun) & isFALSE(upload_spreadOut)) {
  if (!file.exists(fspreadOut)) {
    googledrive::drive_download(file = as_id(gid_spreadOut), path = fspreadOut)
  }
  spreadOut <- loadSimList(fspreadOut)
} else {
  spreadOut <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = spreadFitParams,
    modules = "fireSense_SpreadFit",
    paths = spreadFitPaths,
    objects = spreadFitObjects
  )
  saveSimList(spreadOut, fspreadOut, fileBackend = 2)

  if (isTRUE(upload_spreadOut)) {
    if (!dir.exists(tempdir())) {
      dir.create(tempdir()) ## TODO: why is this dir being removed in the first place?
    }
    fdf <- googledrive::drive_put(media = fspreadOut, path = as_id(gdriveURL), name = basename(fspreadOut))
    gid_spreadOut <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "spreadOut", runID = run,
                 gcm = NA, ssp = NA, gid = gid_spreadOut),
      gdriveSims
    )
  }

  source("R/upload_spreadFit.R")

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("`fireSense_SpreadFit` for `", runName, "` completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
  }
}
