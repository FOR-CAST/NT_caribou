mergePredictedCaribouTables <- function(pathToFiles,
                                        climateScenarios,
                                        yr,
                                        runs,
                                        studyAreaName,
                                        herdSHP){
Require("usefulFuns")
allTbs  <- grepMulti(list.files(dirname(Paths$outputPath),
                                           recursive = TRUE,
                                           full.names = TRUE),
                                patterns = c("predictedCaribou_Year",
                                             paste(climateScenarios, collapse = "|"),
                                             ".rds"))

DTcs <- rbindlist(lapply(climateScenarios, function(eachClim){
      DTr <- rbindlist(lapply(runs, function(RUN){
      # Search for the matching string in allTbs
      whichDT <- grepMulti(x = allTbs, patterns = c(yr[length(yr)],
                                                    eachClim,
                                                    RUN))
      if (length(whichDT) > 1){
        message("length(whichDT) > 1. Please debug")
        browser()
      }
      DT <- readRDS(whichDT)
        DTy <- rbindlist(lapply(paste0("Year", yr), function(eachYear){
          DTy <- DT[[eachYear]]
          DTy[, Year := as.numeric(substrBoth(strng = eachYear, howManyCharacters = 4, fromEnd = TRUE))]
        }))
        # Add run to DT
        DTy[, Run := RUN]
        return(DTy)
      }))
    # Add Climate Scenarios
    cM <- strsplit(x = eachClim, split = paste0(studyAreaName, "_"))[[1]][2]
    if (is.na(cM)){
      print("climate model is NA. Please debug")
      browser()
    }
    DTr[, climateModel := cM]
    return(DTr)
}))

return(DTcs)

}
