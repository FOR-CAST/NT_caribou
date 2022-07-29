makeStudyArea_NT_caribou <- function(rasterToMatch,
                                     pathSA){

  # We need to use all three shapefiles we got from ENR-NT for producing
  # the estimated for caribouPopGrowth

  DT <- data.table(SHP = c("GNWT_NT1_range",
                           "RangePlanRegions",
                           "Revised_Study_Areas"),
                   archve = c("BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020.zip",
                                "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.zip",
                                "Boreal_Caribou_Revised_Study_Areas_2020.zip"),
                   fileName = c("BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020.shp",
                                "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                                "Boreal_Caribou_Revised_Study_Areas_2020.shp"),
                   url = c("https://drive.google.com/file/d/1VRSolnXMYPrkdBhNofeR81dCu_NTBSgf/view?usp=sharing",
                           "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3/view?usp=sharing",
                           "https://drive.google.com/file/d/1FNQKCjKhZIsr5rzWGfTvJ74K2KexN3um/view?usp=sharing"))

  allSA <- lapply(c("GNWT_NT1_range",
                    "RangePlanRegions",
                    "Revised_Study_Areas"), function(shp) {
    theShape <- DT[SHP == shp, ]
    S <- prepInputs(url = theShape[["url"]],
               destinationPath = checkPath(file.path(Paths$inputPath, "caribou"),
                                           create = TRUE),
                      archive = theShape[["archve"]],
                      alsoExtract = "similar",
                      # alsoExtract = paste0(SHP, c(".dbf", ".shx",
                      #                             ".sbn", ".sbx",
                      #                             ".shp.xml", ".prj")),
                      targetFile = theShape[["fileName"]])

    # Reproject to rasterToMatch
    Srepj <- projectInputs(x = S, targetCRS = crs(rasterToMatch))
    return(Srepj)
  })

  names(allSA) <- DT[["SHP"]]

  studyArea <- sf::st_union(allSA$GNWT_NT1_range, allSA$RangePlanRegions)
  studyArea <- sf::st_union(studyArea, allSA$Revised_Study_Areas)

  all <- list(studyArea = studyArea,
              listSACaribou = allSA)

  # save to pathSA
  qs::qsave(x = all, file = pathSA)

  return(all)

  }
