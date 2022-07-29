if (file.exists(".Renviron")) readRenviron(".Renviron")

years <- c(2011, 2100)
studyAreaNames <- c("NT1_BCR6")
climateScenarios <- c("CanESM5_SSP370", "CanESM5_SSP585", "CNRM-ESM2-1_SSP370", "CNRM-ESM2-1_SSP585")

runName <- sprintf("%s_%s_run01", studyAreaNames[1], climateScenarios[1]) ## need a runName for gids

source("01-packages.R")
source("02-init.R")
source("03-paths.R")
source("04-options.R"); options(mc.cores = nReps);
source("05-google-ids.R")

usePrerun <- TRUE
doUpload <- TRUE

do.call(setPaths, posthocPaths)

posthocModules <- list("Biomass_summary", "fireSense_summary")

source("modules/caribouNT_studyArea/R/sppEquiv.R") ## makeSppEquivWBI()

parallel::mclapply(studyAreaNames, function(sAN) {
  gid_results <- gdriveSims[studyArea == sAN & simObject == "results", gid]
  names(gid_results) <- sAN

  ## params
  posthocParams <- list(
    Biomass_summary = list(
      climateScenarios = climateScenarios,
      simOutputPath = dirname(defaultPaths$outputPath), ## "outputs"
      studyAreaNames = sAN,
      reps = nReps,
      upload = doUpload,
      year = years
    ),
    fireSense_summary = list(
      climateScenarios = climateScenarios,
      simOutputPath = dirname(defaultPaths$outputPath), ## "outputs"
      studyAreaNames = sAN,
      reps = nReps,
      upload = doUpload
    )
  )

  ## objects
  sppEquiv <- makeSppEquivWBI(sAN)
  treeSpecies <- unique(sppEquiv[, c("LandR", "Type")])
  setnames(treeSpecies, "LandR", "Species")

  sim_SA <- loadSimList(file.path("outputs", sAN,
                                  paste0("simOutPreamble_", sAN, "_",
                                         gsub("SSP", "", climateScenarios[[1]]), ".qs")))
  rasterToMatch <- sim_SA$rasterToMatchReporting
  rm(sim_SA)

  posthocObjects <- list(
    rasterToMatch = rasterToMatch,
    treeSpecies = treeSpecies,
    uploadTo = gid_results
  )

  posthocSim <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = posthocParams,
    modules = posthocModules,
    loadOrder = unlist(posthocModules),
    objects = posthocObjects,
    paths = posthocPaths
  )

  TRUE
})
