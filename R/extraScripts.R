# removed from summarizeCaribou
# 1. RSF Static vs Dynamic results
# 2. Average biomass and leading maps


#### Checking Boo Static vs Dynamic
# booStatic <- raster::raster("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS_with2001/CCSM4_run1/caribouPredictions/relativeSelectioncaribouRSF_NT_Year2017_static.tif")
# booDynamic <- raster::raster("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/CCSM4_run1/caribouPredictions/relativeSelectioncaribouRSF_NT_Year2017.tif")
#
# # Apply the binning to both
# binningTable <- Cache(prepInputs,
#                       targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
#                       url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
#                       destinationPath = tempdir(),
#                       fun = "data.table::fread",
#                       userTags = c("object:binningTable", "goal:finalPlots"))
#
# reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
#                               binningTable[["Max.Value"]],
#                               binningTable[["RSF.Bin"]]),
#                         ncol = 3)
# # Make sure that the max value is infinite, so it accommodates any bigger value
# # than before
# reclassMatrix[nrow(reclassMatrix), 2] <- Inf
# booStaticBin <- raster::reclassify(x = booStatic, rcl = reclassMatrix)
# booDynamicBin <- raster::reclassify(x = booDynamic, rcl = reclassMatrix)
# diffClass <- booDynamicBin-booStaticBin
#
# # hist(diffClass, xlab = "RSF Bins", main = "",
# #      ylab = "Frequency of pixels (x 10\u2076)",
# #      xlim = c(-9, 9))
#
# tb <- table(diffClass[])
# tbReady <- data.table(xAxis = factor(rownames(tb),
#                                      levels = as.character(-9:9)),
#                       yAxis = as.numeric(tb))
# palH <- colorRampPalette(RColorBrewer::brewer.pal(length(-9:9), name = "Spectral"))(length(-9:9))
# ph <- ggplot(tbReady, aes(x = xAxis, y = yAxis/(10^6))) +
#   xlab("RSF Bins") +
#   ylab("Frequency of pixels (x 10\u2076)") +
#   geom_col(fill = palH, col = "black") +
#   theme_classic(base_size = 16)
# ph
#
# ggsave(file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots/histogramComparison.png"),
#        width = 12,
#        height = 8)
#
#
# library("lattice")
# library("rasterVis")
# library("viridis")
# library("maptools")
#
# shpLoaded <- maptools::readShapeLines("~/projects/NWT/inputs/NWT_NT1_BCR6_2011/RSFshp.shp")
#
# # Add shp to levelplot
# nb.cols <- length(-9:9)
# Pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(nb.cols)
# Ras <- round(diffClass, 0)
# rasBinned <- ratify(Ras)
# att <- "ID"
#
# png(filename = file.path(getwd(), "outputs/caribou/differenceRSFMaps.png"),
#     width = 21, height = 29,
#     units = "cm", res = 300)
#
# print(rasterVis::levelplot(rasBinned,
#                            sub = paste0("Differences in caribou RSF predictions using ",
#                                         "static and dynamic land cover"),
#                            att = att,
#                            margin = FALSE,
#                            maxpixels = 6e6,
#                            colorkey = list(
#                              space = 'bottom',
#                              at = -9:9,
#                              axis.line = list(col = 'black'),
#                              width = 0.75
#                            ),
#                            par.settings = list(
#                              strip.border = list(col = 'transparent'),
#                              strip.background = list(col = 'transparent'),
#                              axis.line = list(col = 'transparent')),
#                            scales = list(draw = FALSE),
#                            col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
#                            par.strip.text = list(cex = 0.8,
#                                                  lines = 1,
#                                                  col = "black"),
#                            panel = function(...){
#                              lattice::panel.levelplot.raster(...)
#                              sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
#                            }))
#
# dev.off()
#
# ###########################
# ###########################
#
# # Make average 2011 and 2100 biomass and leading maps
#
# source('~/projects/NWT/posthocFunctions/bioLeadMaps.R')
# pathData <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS"
#
# treeSpecies = c("Betu_Pap","Lari_Lar","Pice_Gla",
#                 "Pice_Mar","Pinu_Ban","Popu_Tre")
#
# leadingPercentage <- 0.75
#
# pal <- c("#27408B", "#8B7D6B", "#CD0000", "#EEB422", "#9A32CD", "#006400",
#          "#A6BFFF", "#F1E3D1", "#FF7F7F", "#FFDC66", "#FFB1FF", "#7FE37F")
#
# Require("maptools")
#
# shpLoaded <- maptools::readShapeLines("~/projects/NWT/inputs/NWT_NT1_BCR6_2011/RSFshp.shp")
#
# lead2011_CCSM4 <- bioLeadMaps(Year = 2011,
#                               run = "run1",
#                               climateScenario = "CCSM4",
#                               pathData,
#                               shp = shpLoaded,
#                               pal = pal,
#                               pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                               leadingPercentage = 0.75)
#
# lead2100_CCSM4 <- bioLeadMaps(Year = 2100,
#                               run = "run1",
#                               climateScenario = "CCSM4",
#                               pathData,
#                               shp = shpLoaded,
#                               pal = pal,
#                               pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                               leadingPercentage = 0.75)
#
# lead2011_CanESM2 <- bioLeadMaps(Year = 2011,
#                                 run = "run2",
#                                 climateScenario = "CanESM2",
#                                 pathData,
#                                 shp = shpLoaded,
#                                 pal = pal,
#                                 pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                                 leadingPercentage = 0.75)
#
# lead2100_CanESM2 <- bioLeadMaps(Year = 2100,
#                                 run = "run2",
#                                 climateScenario = "CanESM2",
#                                 pathData,
#                                 shp = shpLoaded,
#                                 pal = pal,
#                                 pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                                 leadingPercentage = 0.75)
#
# lead2011_INM_CM4 <- bioLeadMaps(Year = 2011,
#                                 run = "run3",
#                                 climateScenario = "INM-CM4",
#                                 pathData,
#                                 shp = shpLoaded,
#                                 pal = pal,
#                                 pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                                 leadingPercentage = 0.75)
#
# lead2100_INM_CM4 <- bioLeadMaps(Year = 2100,
#                                 run = "run3",
#                                 climateScenario = "INM-CM4",
#                                 pathData,
#                                 shp = shpLoaded,
#                                 pal = pal,
#                                 pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
#                                 leadingPercentage = 0.75)


