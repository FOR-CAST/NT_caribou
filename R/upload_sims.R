source("01a-packages-libPath.R")

library("Require")
Require(c("data.table", "googledrive", "purrr"))

token <- Require::normPath(list.files(".", "western-boreal-initiative-.*[.]json")[1])
haveToken <- isTRUE(length(token) == 1)

if (haveToken) {
  drive_auth(path = token)
} else {
  message(crayon::red("No Google service account token found. Trying user authentication..."))
  drive_auth(email = "achubaty@for-cast.ca", use_oob = quickPlot::isRstudioServer())
}

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))


## upload -------------------------------------------------------------------------------------

sA <- studyAreaName <- "NT1_BCR6"

source("05-google-ids.R")

gid_results <- as_id(gdriveSims[studyArea == sA & simObject == "results", gid])

files2upload <- list.files("outputs", paste0(sA, "_(CanESM5|CNRM-ESM2-1).*[.]tar.gz$"), full.names = TRUE)
files2upload <- set_names(files2upload, basename(files2upload))

prevUploaded <- drive_ls(gid_results)
toUpload <- files2upload[!(basename(files2upload) %in% prevUploaded$name)]
uploaded <- map(toUpload, ~ drive_upload(.x, path = gid_results))
