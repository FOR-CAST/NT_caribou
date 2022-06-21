# source("01a-packages-libPath.R")
source("01-packages.R")
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

Require(c("caribouMetrics", "raster", "sf", "tictoc", "usefulFuns"))

source("02-init.R")

scratchDirOrig <- scratchDir
source("03-paths.R")

source("04-options.R")
maxLimit <- 20000 # in MB
options(
  future.globals.maxSize = maxLimit*1024^2, ## we use ~6 GB for layers here
  NCONNECTIONS = 120L  ## R cannot exceed 125 connections; use fewer to be safe
)

source("05-google-ids.R")

nodeName <- Sys.info()[["nodename"]]
studyAreaNames <- c("NT1")
wildlifeModules <- list("caribouPopGrowthModel") ## TODO: caribouRSF_NT
climateGCMs <- c("CanESM5", "CNRM-ESM2-1")
climateSSPs <- c("SSP370", "SSP585")

for (RP in c(paste0("run0", 1:nReps))) {
  for (CS in climateGCMs) {
    for (SS in climateSSPs) {
      for (P in studyAreaNames) {
        fls <- list.files(file.path("outputs", P, "posthoc"))
        if (length(fls) != 0) {
          ## TODO: is this same as the grepMulti in usefulFuns? conflicts???
          grepMulti <- function(x, patterns, unwanted = NULL) {
            rescued <- sapply(x, function(fun) all(sapply(X = patterns, FUN = grepl, fun)))
            recovered <- x[rescued]
            if (!is.null(unwanted)) {
              discard <- sapply(recovered, function(fun) all(sapply(X = unwanted, FUN = grepl, fun)))
              afterFiltering <- recovered[!discard]
              return(afterFiltering)
            } else {
              return(recovered)
            }
          }

          allFls <- grepMulti(fls,
                              patterns = c(RP, CS, SS, P),
                              unwanted = ".aux.xml")
          if (length(allFls) == 115*5) {
            message(crayon::green(paste0("Simulations done for:", paste(P, SS, CS, RP, collapse = " "))))
            next
          }
        }
        message(crayon::yellow(paste0("Simulations starting for:", paste(P, SS, CS, RP, collapse = " "))))

        runName <- paste(P, CS, SS, RP, sep = "_")
        studyAreaName <- P
        scratchDir <- scratchDirOrig
        source("03-paths.R") ## reset paths for runName

        do.call(setPaths, posthocPaths)

        ## if a study area is already complete, skip it and do next one
        donefile <- file.path(posthocPaths[["outputPath"]], paste0("00-DONE_", paste(P, SS, CS, RP, sep = "_")))
        if (file.exists(donefile)) {
          message("Postprocessing for study area ", studyAreaName, " previously completed. Skipping.")
          next
        }

        ## if a study area is already being processed in another R session, skip it and do next one
        lockfile <- file.path(posthocPaths[["outputPath"]], paste0("00-LOCK_", studyAreaName))
        if (file.exists(lockfile)) {
          message("Found lockfile for study area ", studyAreaName, ". Skipping.")
          next
        } else {
          file.create(lockfile)
          on.exit({unlink(lockfile)}, add = TRUE)
        }

        tic(paste0("Finished for ", runName, ". ELAPSED TIME: "))

        stepCacheTag <- c(paste0("cache:10b"),
                          paste0("runName:", runName))

        # Derive parameters from runName
        scenario <- runName
        Run <- strsplit(runName, split = "_")[[1]][4]
        Province <- strsplit(runName, split = "_")[[1]][1]
        ClimateModel <- strsplit(runName, split = "_")[[1]][2]
        SSP <- strsplit(runName, split = "_")[[1]][3]
        # Determine study area long name
        studyAreaLongName <- switch(studyAreaName, ## TODO: update for new study area module
                                    AB = "Alberta",
                                    BC = "British Columbia",
                                    SK = "Saskatchewan",
                                    MB = "Manitoba",
                                    NT = "Northwest Territories & Nunavut",
                                    NU = "Northwest Territories & Nunavut",
                                    YT = "Yukon",
                                    RIA = "RIA")

        # RTM
        pathRTM <- file.path(posthocPaths[["inputPath"]], paste0(studyAreaName, "_rtm.tif"))

        if (file.exists(pathRTM)) {
          rasterToMatch <- raster(pathRTM)
        } else stop("RTM doesn't exist. Please run script 'source('06-studyArea.R')'")

        # Make RTM have only 1's
        rasterToMatch[!is.na(rasterToMatch)] <- 1

        # STUDY AREA

        pathSA <- file.path(posthocPaths[["inputPath"]], paste0(studyAreaName, "_SA.qs"))

        if (file.exists(pathSA)) {
          studyArea <- qs::qread(pathSA)
        } else {
          studyArea <- makeStudyArea(studyAreaName)
        }

        ## Calculate number of cores and divide in groups if needed
        birdSpecies <- checkBirdsAvailable_WBI(whichRun = Run) ## TODO: remove all bird stuff

        cores <- if (NROW(birdSpecies) < 4) {
          NROW(birdSpecies)
        } else {
          birdPredictionCoresCalc_WBI(birdSpecies = birdSpecies[["Species"]], sizeGbEachProcess = 8)
        }

        # Defining model version
        if (!exists("birdModelVersion")) birdModelVersion <- c("reducedBAM") # Default if not provided
        predictionInterval <- 20

        urlStaticLayers <- "https://drive.google.com/drive/u/0/folders/1RPXqgq-M1mOKMYzUnVSpw_6sjJ4m07dj"

        pixelsWithDataAtInitialization <- Cache(loadStaticLayers,
                                                fileURL = urlStaticLayers, # Add Cache when fun is ready
                                                pathData = posthocPaths[["inputPath"]],
                                                studyArea = studyArea,
                                                rasterToMatch = rasterToMatch,
                                                Province = Province,
                                                version = birdModelVersion,
                                                allVariables = "Structure_Biomass_TotalLiveAboveGround_v1",
                                                staticLayersNames = "Structure_Biomass_TotalLiveAboveGround_v1",
                                                userTags = c(stepCacheTag, "objectName:pixelsWithDataAtInitialization"))

        pixelsWithDataAtInitialization <- which(pixelsWithDataAtInitialization[] != 0)

        ############ WATER and WETLAND PREP WITH LCC LAYER #########

        # Latifovic, R., Pouliot, D., and Olthof, I., (2017) Circa 2010 Land Cover of Canada: Local Optimization Methodology and Product
        # Development. Remote Sensing, 2017, 9(11), 1098; http://www.mdpi.com/2072-4292/9/11/1098, describes the methodology Canada used to
        # produce the Landcover of Canada data included in this data set.
        #
        # The following list describes the display of land cover classification in the .tif file:
        # Value 1, Temperate or sub-polar needleleaf forest, RGB 0 61 0;
        # Value 2, Sub-polar taiga needleleaf forest, RGB 148 156 112;
        # Value 3, Tropical or sub-tropical broadleaf evergreen forest, RGB 0 99 0;
        # Value 4, Tropical or sub-tropical broadleaf deciduous forest, RGB 30 171 5;
        # Value 5, Temperate or sub-polar broadleaf deciduous forest, RGB 20 140 61;
        # Value 6, Mixed forest, RGB 92 117 43;
        # Value 7, Tropical or sub-tropical shrubland, RGB 179 158 43;
        # Value 8, Temperate or sub-polar shrubland, RGB 179 138 51;
        # Value 9, Tropical or sub-tropical grassland, RGB 232 220 94;
        # Value 10, Temperate or sub-polar grassland, RGB 225 207 138;
        # Value 11, Sub-polar or polar shrubland-lichen-moss, RGB 156 117 84;
        # Value 12, Sub-polar or polar grassland-lichen-moss, RGB 186 212 143;
        # Value 13, Sub-polar or polar barren-lichen-moss, RGB 64 138 112;
        # Value 14, Wetland, RGB 107 163 138;
        # Value 15, Cropland, RGB 230 174 102;
        # Value 16, Barren lands, RGB 168 171 174;
        # Value 17, Urban, RGB 220 33 38;
        # Value 18, Water, RGB 76 112 163;
        # Value 19, Snow and Ice, RGB 255 250 255.

        # Important ones:
        waterValues <- 18
        wetlandValues <- 14
        forestValues <- 1:6 # Forests
        uplandValues <- c(forestValues,
                          7:13, # Shrubland, grassland
                          15:17, # Cropland, barren and Urban
                          19) # Ice and snow

        landcoverMap <- Cache(LandR::prepInputsLCC, destinationPath = posthocPaths[["inputPath"]],
                              studyArea = studyArea,
                              rasterToMatch = rasterToMatch,
                              filename2 = paste0("LCC_", Province, ".tif"),
                              userTags = c("objectName:landcoverMap",
                                           stepCacheTag,
                                           "outFun:Cache"),
                              omitArgs = c("destinationPath", "filename2"))

        watersVals <- raster::getValues(landcoverMap)

        watersValsToChange <- watersVals
        watersValsToChange[!is.na(watersValsToChange) & !watersValsToChange %in% waterValues] <- NA
        waterRaster <- raster::setValues(x = landcoverMap, watersValsToChange)
        waterRaster[!is.na(waterRaster)] <- 1

        watersValsToChange <- watersVals
        watersValsToChange[!is.na(watersValsToChange) & !watersValsToChange %in% wetlandValues] <- NA
        wetlandsRaster <- raster::setValues(x = landcoverMap, watersValsToChange)
        wetlandsRaster[!is.na(wetlandsRaster)] <- 1

        watersValsToChange <- watersVals
        watersValsToChange[!is.na(watersValsToChange) & !watersValsToChange %in% uplandValues] <- NA
        uplandsRaster <- raster::setValues(x = landcoverMap, watersValsToChange)
        uplandsRaster[!is.na(uplandsRaster)] <- 1

        watersValsToChange <- watersVals
        watersValsToChange[!is.na(watersValsToChange) & !watersValsToChange %in% forestValues] <- NA
        forestOnly <- raster::setValues(x = landcoverMap, watersValsToChange)
        forestOnly[!is.na(forestOnly)] <- 1

        #bSpG <- birdSpecies[Species %in% cores[["birdSpecies"]][[groupID]], Species] ## TODO: revisit
        bSpG <- birdSpecies[, Species]

        # Add Parameters
        parameters <- list(
          birdsNWT = list(
            "predictLastYear" = FALSE,
            "lowMem" = TRUE,
            "scenario" = scenario, # composed by 2letterProvince_climateModel_SSP_runX
            "useStaticPredictionsForNonForest" = TRUE,
            "useOnlyUplandsForPrediction" = TRUE,
            "baseLayer" = 2010,
            "overwritePredictions" = FALSE,
            "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from
            # LandR_Biomass simulations to run it with
            "predictionInterval" = predictionInterval,
            "nCores" = length(bSpG), #"auto", # If not to parallelize, use 1
            "version" = birdModelVersion,
            "RCP" = SSP,
            "climateModel" = ClimateModel,
            "climateResolution" = NULL,
            "climateFilePath" = NULL
          ),
          caribouPopGrowthModel = list(
            ".plotInitialTime" = NULL,
            "climateModel" = ClimateModel,
            "useFuture" = FALSE,
            "recoveryTime" = 40,
            ".useDummyData" = FALSE,
            ".growthInterval" = 10,
            "recruitmentModelVersion" = "Johnson", # Johnson or ECCC
            "recruitmentModelNumber" = "M4",
            "femaleSurvivalModelNumber" = c("M1", "M4") # M1:M5 --> best models: M1, M4
          )
          # ATTENTION: recruitmentModelNumber and recruitmentModelVersion need to be paired. ie.
          # if you want to run M3 from ECCC and M1 and M4 from Johnson you should put these as
          #     "recruitmentModelVersion" = c("ECCC", "Johnson", "Johnson"),
          #     "recruitmentModelNumber" = c("M3", "M1", "M4"),
          # otherwise it will repeat the recruitmentModelVersion for all recruitmentModelNumber
        )

        # Modules
        modules <- wildlifeModules ## defined at top of script

        # Set simulation times
        Times <- list(start = 2011, end = 2091)

        # Setting some inputs before changing input path
        zipClimateDataFilesFolder <- file.path(posthocPaths[["inputPath"]], "climate", "future")
        climateDataFolder <- checkPath(file.path(posthocPaths[["inputPath"]],
                                                 "climate", "future", "climate_MSY"),
                                       create = TRUE)

        # Reset input paths to the folder where simulation outputs are
        posthocPaths[["inputPath"]] <- file.path("outputs", runName) # THIS IS THE ORIGINAL FOR WHEN THE RUNS ARE DONE

        rstCurrentBurnList <- rstCurrentBurnListGenerator_WBI(pathInputs = posthocPaths[["inputPath"]])

        # Add objects
        objects <- list(
          "studyArea" = studyArea,
          "rasterToMatch" = rasterToMatch,
          "usrEmail" = config::get("cloud")[["googleuser"]],
          "waterRaster" = waterRaster,
          "wetlandsRaster" = wetlandsRaster,
          "uplandsRaster" = uplandsRaster,
          "zipClimateDataFilesFolder" = zipClimateDataFilesFolder,
          "climateDataFolder" = climateDataFolder, # Currently here, but should be moved to below
          "pixelsWithDataAtInitialization" = pixelsWithDataAtInitialization,
          "studyAreaLongName" = studyAreaLongName, # For annual climate variables
          "urlStaticLayers" = urlStaticLayers,
          "urlModels" = birdSpecies, # birdSpecies[Species %in% cores[["birdSpecies"]][["Group1"]], ],
          "birdsList" = bSpG,
          "rstLCC" = landcoverMap,
          "sppEquiv" = LandR::sppEquivalencies_CA, # Loading species equivalency table
          "sppEquivCol" = "KNN",
          "forestOnly" = forestOnly,
          "rstCurrentBurnList" = rstCurrentBurnList,
          "runName" = runName,
          "shortProvinceName" = Province
        )

        outputsBoo <- data.frame(objectName = c("predictedCaribou",
                                                "disturbances"),
                                 file = c(paste0("predictedCaribou_Year2091_", runName),
                                          paste0("disturbances_Year2091_", runName)),
                                 saveTime = Times$end)

        message(crayon::yellow(paste0("Starting simulations for BIRDS and BOO using ",
                                      paste(ClimateModel, SSP, collapse = " "),
                                      " for ", Province, " (", Run, ")")))

        simOut <- simInitAndSpades(times = Times,
                                   params = parameters,
                                   modules = modules,
                                   objects = objects,
                                   paths = posthocPaths,
                                   outputs = outputsBoo,
                                   loadOrder = unlist(modules))

        toc()

        file.create(donefile)

        if (file.exists(lockfile)) unlink(lockfile)
      }
    }
  }
}
