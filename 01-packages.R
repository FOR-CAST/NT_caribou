source("01a-packages-libPath.R")

if (!"Require" %in% rownames(installed.packages())) {
  remotes::install_github("PredictiveEcology/Require@development")
} else if (packageVersion("Require") < "0.1.0.9000") {
  remotes::install_github("PredictiveEcology/Require@development")
}
library(Require)

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "stars", "raster", "terra")

Require("PredictiveEcology/SpaDES.install@development")

installSpaDES(dontUpdate = .spatialPkgs, upgrade = "never")

if (FALSE) {
  installSpatialPackages()
  #install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

## WORKAROUND: missing CRAN packages + related errors
if (FALSE) {
  install.packages("https://cran.r-project.org/src/contrib/Archive/gdalUtils/gdalUtils_2.0.3.2.tar.gz", repos = NULL)
  install.packages("https://cran.r-project.org/src/contrib/Archive/RandomFields/RandomFields_3.3.14.tar.gz", repos = NULL)
  install.packages(c("foreach", "R.utils")) ## TODO: RandomFields ?
  remotes::install_github("PredictiveEcology/LandR@development")
}

out <- makeSureAllPackagesInstalled(modulePath = "modules")

Require(c("config", "RCurl", "RPostgres", "tictoc", "XML"), require = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr",
          "PredictiveEcology/reproducible@development (>= 1.2.8.9040)",
          "PredictiveEcology/LandR@development", ## TODO: workaround weird raster/sf method problem
          "PredictiveEcology/SpaDES.core@development (>= 1.0.10.9003)",
          "archive", "googledrive", "httr", "slackr"), upgrade = FALSE)
