if (file.exists(".Renviron")) readRenviron(".Renviron")

# source("01-packages-libPath.R")
source("01-packages.R")

Require(c("caribouMetrics", "ggplot2", "googledrive", "gridExtra",
          "raster", "RColorBrewer", "Rmisc", "sf", "tictoc", "usefulFuns"))

source("02-init.R")
reupload <- FALSE
usePrerun <- TRUE

climateGCMs <- c("CanESM5", "CNRM-ESM2-1")
climateSSPs <- c("SSP370", "SSP585")
nodeName <- Sys.info()[["nodename"]]
studyAreaNames <- "NT1_BCR6"
wildlifeModules <- list("caribouPopGrowthModel", "caribouRSF_NT")

scratchDirOrig <- scratchDir
source("03-paths.R")
source("04-options.R")
source("05-google-ids.R")

do.call(setPaths, summaryPaths)

binningTable <- Cache(prepInputs,
                      targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                      url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                      destinationPath = Paths$outputPath,
                      fun = "data.table::fread",
                      userTags = c("object:binningTable", "goal:finalPlots"))

source("R/makeStudyArea_NT_caribou.R")

pathRTM <- file.path(summaryPaths[["inputPath"]], paste0(studyAreaName, "_rtm.tif"))

if (file.exists(pathRTM)) {
  rasterToMatch <- raster(pathRTM)
} else {
  stop("RTM doesn't exist. Please run script 'source('06-studyArea.R')'")
}

## Make RTM have only 1's
rasterToMatch[!is.na(rasterToMatch)] <- 1

## STUDY AREA
pathSA <- file.path(summaryPaths[["inputPath"]], paste0(studyAreaName, "_SA.qs"))

if (file.exists(pathSA)) {
  studyArea <- qs::qread(pathSA)
} else {
  studyArea <- makeStudyArea_NT_caribou(rasterToMatch = rasterToMatch, pathSA = pathSA)
}

listSACaribou <- studyArea$listSACaribou
studyArea <- studyArea$studyArea

gID <- gid_resultsCaribou
RUNS <- sprintf("run%02d", 1:nReps)
climMods <- data.table(expand.grid(c("CanESM5", "CNRM-ESM2-1"), c("SSP370", "SSP585")))
climMods[, climM := paste0(Var1, "_", Var2)]
climMods <- climMods[["climM"]]
climMods <- paste0(studyAreaNames, "_", climMods)
YS <- seq(2011, 2091, by = 20)

################### RSF #################

# Make Caribou RSF map
# This will be a difference (2100-2011) map, averaged across all reps
# and then across all simulations. Need also to summarize by polygons and
# return in a table

lapply(names(listSACaribou), function(SHP){
  booSHP <- listSACaribou[[SHP]]

Require("RColorBrewer")
source('R/convertShpToRas.R')
source('R/makeCaribouRSFAverageMapSameFolder.R')

booRSF <- makeCaribouRSFAverageMapSameFolder(resultsFolder = file.path(Paths$outputPath, "caribouRSF_predictions"),
                                             runs = RUNS,
                                             runName = SHP,
                                             climateModels = climMods,
                                             outputsPath = file.path(Paths$outputPath, "caribouPlots"),
                                             shp = booSHP,
                                             binningTable = binningTable,
                                             initialYear = 2011,
                                             lastYear = 2091)

meanPolys <- booRSF[["meanRSFPoly"]]

Require("Rmisc")
meanPolys[, upperCI := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["upper"]], by = c("Area", "climateModel")]
meanPolys[, lowerCI := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["lower"]], by = c("Area", "climateModel")]
meanPolys[, sdRSF := sd(RSF, na.rm = TRUE), by = c("Area", "climateModel")]
meanPolys[, meanRSF := mean(RSF, na.rm = TRUE), by = c("Area", "climateModel")]

meanPolys[, sdRSFall := sd(RSF, na.rm = TRUE), by = c("Area")]
meanPolys[, meanRSFall := mean(RSF, na.rm = TRUE), by = c("Area")]
meanPolys[, upperCIall := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["upper"]], by = "Area"]
meanPolys[, lowerCIall := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["lower"]], by = "Area"]

sMP <- unique(meanPolys[, c("Area","climateModel",
                            "meanRSF", "sdRSF", "upperCI", "lowerCI",
                            "meanRSFall", "sdRSFall", "upperCIall","lowerCIall")])

polyMeanName <- file.path(Paths$outputPath, "caribouPlots",
                          paste0("meanRSFperPolySummary_", SHP,".qs"))
qs::qsave(x = sMP, file = polyMeanName)

Require("googledrive")
drive_upload(polyMeanName,
             path = as_id(gID))
drive_upload(file.path(Paths$outputPath, "caribouPlots",
                       paste0("meanRSFperPolygon_", SHP,".qs")),
             path = as_id(gID))
})

source("R/convertShpToRas.R")
source("R/makeCaribouRSFAverageMapSameFolder.R")

lapply(names(listSACaribou), function(SHP) {
  booSHP <- listSACaribou[[SHP]]
  booRSF <- makeCaribouRSFAverageMapSameFolder(
    resultsFolder = file.path(dirname(Paths$outputPath), "caribouRSF_predictions"),
    runs = RUNS,
    runName = SHP,
    climateModels = climMods,
    outputsPath = Paths$outputPath,
    shp = booSHP,
    binningTable = binningTable,
    initialYear = 2011,
    lastYear = 2091
  )

  meanPolys <- booRSF[["meanRSFPoly"]]

  Require("Rmisc")
  meanPolys[, upperCI := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["upper"]], by = c("Area", "climateModel")]
  meanPolys[, lowerCI := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["lower"]], by = c("Area", "climateModel")]
  meanPolys[, sdRSF := sd(RSF, na.rm = TRUE), by = c("Area", "climateModel")]
  meanPolys[, meanRSF := mean(RSF, na.rm = TRUE), by = c("Area", "climateModel")]

  meanPolys[, sdRSFall := sd(RSF, na.rm = TRUE), by = c("Area")]
  meanPolys[, meanRSFall := mean(RSF, na.rm = TRUE), by = c("Area")]
  meanPolys[, upperCIall := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["upper"]], by = "Area"]
  meanPolys[, lowerCIall := Rmisc::CI(x = na.omit(RSF), ci = 0.95)[["lower"]], by = "Area"]

  sMP <- unique(meanPolys[, c("Area", "climateModel",
                              "meanRSF", "sdRSF", "upperCI", "lowerCI",
                              "meanRSFall", "sdRSFall", "upperCIall", "lowerCIall")])

  polyMeanName <- file.path(caribouPlotsDir, paste0("meanRSFperPolySummary_", SHP, ".qs"))
  qs::qsave(x = sMP, file = polyMeanName)

  drive_upload(polyMeanName, path = as_id(gID))
  drive_upload(file.path(caribouPlotsDir, paste0("meanRSFperPolygon_", SHP, ".qs")), path = as_id(gID))
})

################### Population Growth #################

### PLOTS

source("R/mergePredictedCaribouTables.R")

caribouPopulationGrowthTable <- mergePredictedCaribouTables(pathToFiles = file.path(dirname(Paths$outputPath), "posthoc"),
                                                            climateScenarios = climMods,
                                                            yr = YS,
                                                            runs = RUNS,
                                                            studyAreaName = studyAreaName,
                                                            herdSHP = SHP)

popGTB <- file.path(Paths$outputPath, "caribouPlots",
                    "populationGrowthTable.csv")

write.csv(x = caribouPopulationGrowthTable, file = popGTB)

drive_upload(media = popGTB, path = as_id(gID))

polIgn <- if (SHP == "Revised_Study_Areas") "Cameron Hills" else NULL
# Is in AB

source("R/plotCaribouPopGrowthMSSD.R")
# lapply over all areas
plotCaribou <- plotCaribouPopGrowthMSSD(caribouPopulationGrowthTable = caribouPopulationGrowthTable,
                                        outputFolder = file.path(Paths$outputPath, "caribouPlots"),
                                        whichPolysToIgnore = polIgn,
                                        valueOfInterest = seq(5, 85, by = 20),
                                        lims = c(0.8, 1.2),
                                        alphy = 0.07)

library("googledrive")
fl <- list.files(path = Paths$outputPath, "caribouPlots",
                 pattern = "caribou_", full.names = TRUE)
lapply(fl, drive_upload, path = as_id(gID))

################### Disturbances #################

allDist <- list.files(Paths$outputPath, pattern = "disturbances_Year2091",
                      full.names = TRUE, recursive = TRUE)

distTable <- rbindlist(lapply(climMods, function(cM){
  distTableR <- rbindlist(lapply(RUNS, function(rs){

    cM <- strsplit(x = cM, split = paste0(studyAreaName, "_"))[[1]][2]
    fl <- grepMulti(x = allDist, patterns = c(rs, cM))
    listDT <- readRDS(fl)
    DT <- rbindlist(lapply(names(listDT), function(Year){
      yearList <- rbindlist(lapply(names(listDT[[Year]]), function(Area){
        areaList <- rbindlist(lapply(names(listDT[[Year]][[Area]]), function(Herd){
          polyList <- data.table(listDT[[Year]][[Area]][[Herd]])
          polyList[, c("Year", "Area", "Herd") := list(as.numeric(
            usefulFuns::substrBoth(strng = Year,
                                   howManyCharacters = 4,
                                   fromEnd = TRUE)),
            Area, Herd)]
          return(polyList)
        }))
        return(areaList)
      }))
      return(yearList)
    }))
    DT[, c("Run", "climateModel") := list(rs, cM)]
    return(DT)
  }))

}))

dTablePath <- file.path(file.path(Paths$outputPath, "caribouPlots"),
                                  "disturbanceTable.csv")
write.csv(x = distTable, file = dTablePath)
drive_upload(dTablePath, path = as_id(gID))


#TODO ALEX TO REVISE BELOW! <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AC


################### Vegetation #################

tic("Total time elapsed for biomass plots: ")

# Get flammable map
flammablePath <- file.path(Paths$inputPath, paste0("flammableRTM_", studyAreaName,".tif"))
if (!file.exists(flammablePath)) {
  stop(paste0("Flammable map doesn't exist for ", studyAreaName, ". Please correct the ",
              "path to it (line above) or run landscape simulations to create it."))
}

flammable <- raster::raster(flammablePath)

source('R/biomassPlotsCaribou.R')
source('R/bringObjectTSqs.R')
pl <- biomassPlotsCaribou(years = c(2011, 2091),
                          pathData = dirname(defaultPaths$outputPath), ## "outputs"
                          pathOutputs = Paths$outputPath,
                          Scenarios = climMods,
                          shpPoly = studyArea,
                          flammableRTM = flammable,
                          runs = RUNS)
toc()

tic("Total time elapsed for leading plots: ")
flammable <- maskInputs(flammable, studyArea)
source('R/leadingSpPlotsCaribou.R')
pl2 <- leadingSpPlotsCaribou(leadingPercentage = 0.75,
                             years = c(2011, 2091),
                             pathData = dirname(defaultPaths$outputPath), ## "outputs"
                             pathOutputs = Paths$outputPath,
                             Scenarios = climMods,
                             treeSpecies = c("Betu_pap", "Lari_lar",
                                             "Pice_gla", "Pice_mar",
                                             "Pinu_ban", "Popu_tre"),
                             runs = RUNS,
                             rasterToMatch = flammable,
                             flammableRTM = flammable)
toc()

################### Fire #################

source('R/plotBurnSummaryRepsCaribou.R')

burnPlot1 <- plotBurnSummaryRepsCaribou(dataPath = dirname(defaultPaths$outputPath), ## "outputs"
                                        typeSim = climMods[1],
                                        lastYear = 2100,
                                        reps = RUNS,
                                        saveAsQS = FALSE,
                                        overwrite = TRUE)
burnPlot2 <- plotBurnSummaryRepsCaribou(dataPath = dirname(defaultPaths$outputPath), ## "outputs"
                                        typeSim = climMods[2],
                                        lastYear = 2100,
                                        saveAsQS = FALSE,
                                        reps = RUNS,
                                        overwrite = TRUE)
burnPlot3 <- plotBurnSummaryRepsCaribou(dataPath = dirname(defaultPaths$outputPath), ## "outputs"
                                        typeSim = climMods[3],
                                        lastYear = 2100,
                                        saveAsQS = FALSE,
                                        reps = RUNS,
                                        overwrite = TRUE)
burnPlot4 <- plotBurnSummaryRepsCaribou(dataPath = dirname(defaultPaths$outputPath), ## "outputs"
                                        typeSim = climMods[4],
                                        lastYear = 2100,
                                        saveAsQS = FALSE,
                                        reps = RUNS,
                                        overwrite = TRUE)

burnPlotAllScenarios <- plotBurnSummaryRepsCaribou(dataPath = dirname(defaultPaths$outputPath), ## "outputs"
                                                   typeSim = climMods,
                                                   lastYear = 2100,
                                                   saveAsQS = FALSE,
                                                   reps = RUNS,
                                                   overwrite = TRUE)
# Upload all files:
fireFiles <- list.files(path = dirname(defaultPaths$outputPath), ## "outputs"
                        pattern = "burnSummary",
                        full.names = TRUE)
vegFiles <- list.files(path = file.path(Paths$outputPath, "vegetationPlots"),
                       pattern = "csv|png", full.names = TRUE)
landFiles <- c(fireFiles, vegFiles)
booFiles <- list.files(path = Paths$outputPath,
                       pattern = "png|csv", full.names = TRUE)
lapply(landFiles, drive_upload, path = as_id(gID))
lapply(booFiles, drive_upload, path = as_id(gID))

