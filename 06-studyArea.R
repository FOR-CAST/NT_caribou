do.call(setPaths, preamblePaths)

gid_preamble <- gdriveSims[studyArea == studyAreaName & simObject == "simOutPreamble" &
                             gcm == climateGCM & ssp == climateSSP, gid]
upload_preamble <- run == 1 & (reupload | length(gid_preamble) == 0)

preambleObjects <- list(
  .runName = runName
)

preambleModules <- list("caribouNT_studyArea", "canClimateData")

preambleParams <- list(
  canClimateData = list(
    .runName = runName,
    .useCache = ".inputObjects",
    climateGCM = climateGCM,
    climateSSP = climateSSP,
    historicalFireYears = 1991:2020,
    studyAreaName = studyAreaName
  ),
  caribouNT_studyArea = list(
    .runName = runName,
    .useCache = ".inputObjects",
    studyAreaName = studyAreaName
  )
)

fsimOutPreamble <- simFile(paste0("simOutPreamble_", studyAreaName, "_", climateGCM, "_", climateSSP),
                           Paths$outputPath, ext = simFileFormat)
if (isTRUE(usePrerun) & isFALSE(upload_preamble)) {
  if (!file.exists(fsimOutPreamble)) {
    message("Downloading preamble simulation results from google drive...")
    googledrive::drive_download(file = as_id(gid_preamble), path = fsimOutPreamble)
  }
  message("Loading preamble simulation results...")
  simOutPreamble <- loadSimList(fsimOutPreamble)
} else {
  message("Running preamble simulations...")
  simOutPreamble <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = preambleParams,
    modules = preambleModules,
    loadOrder = unlist(preambleModules),
    objects = preambleObjects,
    paths = preamblePaths#,
    #useCache = "overwrite",
    #useCloud = useCloudCache,
    #cloudFolderID = cloudCacheFolderID,
    #userTags = c("preamble", studyAreaName)
  )
  saveSimList(sim = simOutPreamble, filename = fsimOutPreamble, fileBackend = 2)

  if (isTRUE(upload_preamble)) {
    fdf <- googledrive::drive_put(media = fsimOutPreamble, path = as_id(gdriveURL), name = basename(fsimOutPreamble))
    gid_preamble <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "simOutPreamble", runID = NA,
                 gcm = climateGCM, ssp = climateSSP, gid = gid_preamble),
      gdriveSims
    )
  }
}

if (isTRUE(firstRunMDCplots)) {
  ggMDC <- fireSenseUtils::compareMDC(
    historicalMDC = simOutPreamble$historicalClimateRasters$MDC,
    projectedMDC = simOutPreamble$projectedClimateRasters$MDC,
    flammableRTM = simOutPreamble$flammableRTM
  )
  fggMDC <- file.path(preamblePaths$outputPath, "figures", paste0("compareMDC_", studyAreaName, "_",
                                                                  climateGCM, "_", climateSSP, ".png"))
  checkPath(dirname(fggMDC), create = TRUE)

  ggplot2::ggsave(plot = ggMDC, filename = fggMDC)

  if (isTRUE(upload_preamble)) {
    googledrive::drive_put(
      media = fggMDC,
      path = unique(as_id(gdriveSims[studyArea == studyAreaName & simObject == "results", gid])),
      name = basename(fggMDC)
    )
  }
}

nSpecies <- length(unique(simOutPreamble$sppEquiv$LandR))
