#########################################################
##          P L O T S    C A R I B O U                 ##
#########################################################

# This script should be run when we have all replicates ready

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
studyAreaNames <- c("NT1_BCR6")
wildlifeModules <- list("caribouPopGrowthModel", "caribouRSF_NT")
climateGCMs <- c("CanESM5", "CNRM-ESM2-1")
climateSSPs <- c("SSP370", "SSP585")

do.call(setPaths, posthocPaths)

SpaDES.core::setPaths(cachePath = Paths$cachePath,
                      outputPath = checkPath(file.path(Paths$outputPath,
                                                       "caribouPlots"),
                                             create = TRUE))
# Make Caribou RSF map
# This will be a difference (2100-2011) map, averaged across all reps
# and then across all simulations. Need also to summarize by polygons and
# return in a table

binningTable <- Cache(prepInputs,
                      targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                      url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                      destinationPath = Paths$outputPath,
                      fun = "data.table::fread",
                      userTags = c("object:binningTable", "goal:finalPlots"))

# Fixing the caribou SHP to use
# Decho N, Decho S, Hay river lowlands, GSA N, and GSA S

source("~/GitHub/NT_caribou/R/makeStudyArea_NT_caribou.R")

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
  studyArea <- makeStudyArea_NT_caribou(rasterToMatch = rasterToMatch,
                                        pathSA = pathSA)
}

listSACaribou <- studyArea$listSACaribou
studyArea <- studyArea$studyArea

# Google Drive Outputs Folder

gID <- "1IpC_u5c7Mluvqdxzk3pue5NFaBHyY6A6"
RUNS <- c(paste0("run0", 1:9), "run10")
climMods <- data.table(expand.grid(c("CanESM5", "CNRM-ESM2-1"), c("SSP370", "SSP585")))
climMods[, climM := paste0(Var1, "_", Var2)]
climMods <- climMods[["climM"]]
climMods <- paste0(studyAreaNames, "_", climMods)
YS <- seq(2011, 2091, by = 20)

################### RSF #################

lapply(names(listSACaribou), function(SHP){
  booSHP <- listSACaribou[[SHP]]

Require("RColorBrewer")
source('R/convertShpToRas.R')
source('R/makeCaribouRSFAverageMapSameFolder.R')

booRSF <- makeCaribouRSFAverageMapSameFolder(resultsFolder = file.path(dirname(Paths$outputPath), "predictions"),
                                             runs = RUNS,
                                             runName = SHP,
                                             climateModels = climMods,
                                             outputsPath = Paths$outputPath,
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

polyMeanName <- file.path(Paths$outputPath, paste0("meanRSFperPolySummary_",
                                                   SHP,".qs"))
qs::qsave(x = sMP, file = polyMeanName)

Require("googledrive")
drive_upload(polyMeanName,
             path = as_id(gID))
drive_upload(file.path(Paths$outputPath, paste0("meanRSFperPolygon_", SHP,".qs")),
             path = as_id(gID))
})

################### RSF #################

################### Population Growth #################

### PLOTS
# As I need to pass the caribou results, I need to load and already correct the names

source("R/mergePredictedCaribouTables.R")

caribouPopulationGrowthTable <- mergePredictedCaribouTables(pathToFiles = dirname(Paths$outputPath),
                                                            climateScenarios = climMods,
                                                            yr = YS,
                                                            runs = RUNS,
                                                            studyAreaName = studyAreaName,
                                                            herdSHP = SHP)

popGTB <- file.path(Paths$outputPath, "populationGrowthTable.csv")

write.csv(x = caribouPopulationGrowthTable,
          file = popGTB)

drive_upload(media = popGTB,
             path = as_id(gID))

polIgn <- if (SHP == "Revised_Study_Areas") "Cameron Hills" else NULL
# Is in AB

source('R/plotCaribouPopGrowthMSSD.R')
# lapply over all areas
plotCaribou <- plotCaribouPopGrowthMSSD(caribouPopulationGrowthTable = caribouPopulationGrowthTable,
                                        outputFolder = Paths$outputPath,
                                        whichPolysToIgnore = polIgn,
                                        valueOfInterest = seq(5, 85, by = 20),
                                        lims = c(0.8, 1.2),
                                        alphy = 0.07)
library("googledrive")
fl <- list.files(path = Paths$outputPath, pattern = "caribou_", full.names = TRUE)
lapply(fl, drive_upload, path = as_id(gID))

################### Population Growth #################


################### Disturbances #################

allDist <- list.files(dirname(Paths$outputPath), pattern = "disturbances_Year2091",
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

dTablePath <- file.path(Paths$outputPath,
                                  "disturbanceTable.csv")

write.csv(x = distTable, file = dTablePath)

Require("googledrive")
drive_upload(dTablePath,
             path = as_id(gID))

################### Disturbances #################

################### Vegetation #################

library("tictoc")
tic("Total time elapsed for biomass plots: ")

# Get flammable map
flammablePath <- file.path(Paths$inputPath, paste0("flammableRTM_", studyAreaName,".tif"))
if (!file.exists(flammablePath))
  stop(paste0("Flammable map doesn't exist for ", studyAreaName, ". Please correct the ",
              "path to it (line above) or run landscape simulations to create it."))

flammable <- raster::raster(flammablePath)

source('R/biomassPlotsCaribou.R')
source('R/bringObjectTSqs.R')
pl <- biomassPlotsCaribou(years = c(2011, 2091),
                          pathData = dirname(dirname(dirname(Paths$outputPath))),
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
                             pathData = dirname(dirname(dirname(Paths$outputPath))),
                             pathOutputs = Paths$outputPath,
                             Scenarios = climMods,
                             treeSpecies = c("Betu_pap", "Lari_lar",
                                             "Pice_gla", "Pice_mar",
                                             "Pinu_ban", "Popu_tre"),
                             runs = RUNS,
                             rasterToMatch = flammable,
                             flammableRTM = flammable)
toc()
################### Vegetation #################

################### Fire #################
Require("ggplot2")
Require("gridExtra")
Require("data.table")
source('R/plotBurnSummaryRepsCaribou.R')
burnPlot1 <- plotBurnSummaryRepsCaribou(dataPath = dirname(dirname(dirname(Paths$outputPath))),
                                           typeSim = climMods[1],
                                           lastYear = 2100,
                                        reps = RUNS,
                                        saveAsQS = FALSE,
                                           overwrite = TRUE)
burnPlot2 <- plotBurnSummaryRepsCaribou(dataPath = dirname(dirname(dirname(Paths$outputPath))),
                                          typeSim = climMods[2],
                                          lastYear = 2100,
                                        saveAsQS = FALSE,
                                        reps = RUNS,
                                          overwrite = TRUE)
burnPlot3 <- plotBurnSummaryRepsCaribou(dataPath = dirname(dirname(dirname(Paths$outputPath))),
                                              typeSim = climMods[3],
                                              lastYear = 2100,
                                        saveAsQS = FALSE,
                                          reps = RUNS,
                                              overwrite = TRUE)
burnPlot4 <- plotBurnSummaryRepsCaribou(dataPath = dirname(dirname(dirname(Paths$outputPath))),
                                              typeSim = climMods[4],
                                              lastYear = 2100,
                                        saveAsQS = FALSE,
                                        reps = RUNS,
                                              overwrite = TRUE)

burnPlotAllScenarios <- plotBurnSummaryRepsCaribou(dataPath = dirname(dirname(dirname(Paths$outputPath))),
                                                   typeSim = climMods,
                                                   lastYear = 2100,
                                                   saveAsQS = FALSE,
                                                   reps = RUNS,
                                                   overwrite = TRUE)
# Upload all files:
fireFiles <- list.files(path = dirname(dirname(dirname(Paths$outputPath))), pattern = "burnSummary",
                        full.names = TRUE)
vegFiles <- list.files(path = file.path(Paths$outputPath, "vegetationPlots"),
                       pattern = "csv|png", full.names = TRUE)
landFiles <- c(fireFiles, vegFiles)
booFiles <- list.files(path = Paths$outputPath,
                       pattern = "png|csv", full.names = TRUE)
lapply(landFiles, drive_upload, path = as_id(gID))
lapply(booFiles, drive_upload, path = as_id(gID))

