bringObjectTSqs <- function (path, rastersNamePattern) {

  message(crayon::green(paste0("Looking for files in ", path,
                               "\nUsing the following pattern(s): ", paste(rastersNamePattern,
                                                                           sep = "\n"))))
  filesToLoad <- grepMulti(x = list.files(path = path, full.names = TRUE),
                           patterns = rastersNamePattern)
  if (length(filesToLoad) > 1) {
    toRemove <- grepl(x = filesToLoad, pattern = "aux")
    filesToLoad <- filesToLoad[!toRemove]
  }

  if (length(filesToLoad) == 0)
    stop("No files in the folder that have this pattern. Did you pass the correct folder and/or patterns?")
  message(crayon::green("Loading the following file(s):"))
  message(crayon::magenta(paste0(" "), paste0(filesToLoad,
                                              sep = "\n")))
  allRas <- lapply(1:length(filesToLoad), function(index) {
    if (grepl(x = filesToLoad[[index]], pattern = ".tif")) {
        eachRas <- raster::raster(filesToLoad[[index]])
    }
    else {
      if (grepl(x = filesToLoad[[index]], pattern = ".rds")) {
          eachRas <- readRDS(filesToLoad[[index]])
      }
      else {
        if (grepl(x = filesToLoad[[index]], pattern = ".qs")) {
            eachRas <- qs::qread(filesToLoad[[index]])
        } else
          stop("The function can ony deal with .tif, .qs or .rds objects for now")
      }
    }
    return(eachRas)
  })
  y <- unlist(lapply(filesToLoad, function(f) {
    nm <- substrBoth(strng = tools::file_path_sans_ext(f),
                     howManyCharacters = 4, fromEnd = TRUE)
  }))
  names(allRas) <- paste0("Year", y)
  return(allRas)
}
