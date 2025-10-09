# Load internal datasets
#Libraries
library(boxr)
#Params
BOX_CLIENT_ID <- Sys.getenv("BOX_CLIENT_ID")
BOX_CLIENT_SECRET <- Sys.getenv("BOX_CLIENT_SECRET")
# lookup table to clean organization names
# boxr::box_fresh_auth() - If you need to reset tokens after changing app settings in the developer console
boxr::box_auth(
  client_id = BOX_CLIENT_ID,
  client_secret = BOX_CLIENT_SECRET
)
boxr::box_dl(
  file_id = "2011395542292",
  local_dir = "data/validate/lookup",
  overwrite = TRUE
)
