cache_conn <- if (config::get("cachedb") == "sqlite") {
  NULL ## default to sqlite
} else if (config::get("cachedb") == "postgresql") {
  Require("RPostgres", require = FALSE)
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = Sys.getenv("PGHOST"),
                 port = Sys.getenv("PGPORT"),
                 dbname = Sys.getenv("PGDATABASE"),
                 user = Sys.getenv("PGUSER"),
                 password = Sys.getenv("PGPASSWORD"))
} else {
  stop("Unsupported cache database type '", config::get("cachedb"), "'")
}

maxLimit <- 20000 # for future; in MB
maxMemory <- 5e+12 # for raster

terra::terraOptions(tempdir = checkPath(file.path(scratchDir, "terra"), create = TRUE))
raster::rasterOptions(default = TRUE)
opts <- options(
  "encoding" = "UTF-8",
  "future.globals.maxSize" = maxLimit*1024^2, ## we use ~6 GB for layers here
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "NCONNECTIONS" = 120L,  ## R cannot exceed 125 connections; use fewer to be safe
  "rasterMaxMemory" = maxMemory,
  "rasterTmpDir" = file.path(scratchDir, "raster"),
  "reproducible.cachePath" = file.path(scratchDir, "cache"),
  "reproducible.cacheSaveFormat" = cacheFormat,
  "reproducible.conn" = cache_conn,
  "reproducible.destinationPath" = normPath(defaultPaths[["inputPath"]]),
  "reproducible.inputPaths" = userInputPaths,
  "reproducible.nThreads" = 2,
  "reproducible.overwrite" = TRUE,
  "reproducible.polygonShortcut" = FALSE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE,
  "reproducible.useMemoise" = useMemoise,
  "reproducible.useNewDigestAlgorithm" = reproducibleAlgorithm,
  "reproducible.useRequire" = useRequire,
  "reproducible.useTerra" = useTerra,
  "spades.messagingNumCharsModule" = messagingNumCharsModule,
  "spades.moduleCodeChecks" = codeChecks,
  "spades.nThreads" = 4,
  "spades.recoveryMode" = FALSE,
  "spades.restartR.restartDir" = defaultPaths[["outputPath"]],
  "spades.scratchPath" = scratchDir,
  "spades.useRequire" = useRequire
)

httr::set_config(httr::config(http_version = 0))
httr::timeout(seconds = 10)

token <- Require::normPath(list.files(".", "western-boreal-initiative-.*[.]json")[1])
haveToken <- all(isTRUE(length(token) == 1), !is.na(token))

if (haveToken) {
  drive_auth(path = token)
} else {
  message(crayon::red("No Google service account token found. Trying user authentication..."))

  googledrivecache <- config::get("cloud")[["googledrivecache"]]
  if (!is.null(googledrivecache)) {
    options(gargle_oauth_cache = googledrivecache)
  }

  drive_auth(email = config::get("cloud")[["googleuser"]], use_oob = quickPlot::isRstudioServer())
}

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
