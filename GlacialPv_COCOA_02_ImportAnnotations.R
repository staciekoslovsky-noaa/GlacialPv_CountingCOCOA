# Import coastal harbor seal annotations to DB
# S. Koslovsky

# Set variables --------------------------------------------------
survey_year <- 2020
survey_id <- 'tracy_20200905_fullmosaic_1' # survey_id for data that were counted

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")

# Process data ---------------------------------------------------

# Set up working environment
"%notin%" <- Negate("%in%")

counted_folder <- paste0("Y://NMML_Polar_Imagery/Surveys_HS/Glacial/Projects/Surveys Glacial Sites Counts/", survey_year, "/", survey_id)
wd <- counted_folder
setwd(wd)

con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"),
                              password = Sys.getenv("user_pw"))

# Delete data from tables, if previously imported
RPostgreSQL::dbSendQuery(con, paste0("DELETE FROM surv_pv_gla.tbl_detections_processed_rgb WHERE detection_id LIKE \'%", survey_id, "%\'"))

# Import data and process
files <- data.frame(processed = list.files(path = wd, full.names = FALSE, recursive = FALSE, pattern = "processed"), stringsAsFactors = FALSE) 

for (f in 1:nrow(files)) {
  processed_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM surv_pv_gla.tbl_detections_processed_rgb") 
  processed_id$max <- ifelse(is.na(processed_id$max), 0, processed_id$max)
  
  processed <- read.csv(files$processed[f], skip = 2, header = FALSE, stringsAsFactors = FALSE, 
                        col.names = c("detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", "poly_cocoa"))
  detection_types <- unique(processed$detection_type)
  
  if (nrow(processed) > 0) {
    if ("needs_review" %in% detection_types == TRUE) {
      stop(paste0("Not all needs_review areas have been reviewed or updated in ", files$processed[f]))
    }
    
    if (length(detection_types[!(detection_types %in% c("harbor_seal", "harbor_pup", "reviewed", "ignore", "suppressed", "no_seals"))]) > 0) {
      stop(paste0("Unexpected detection_type values in ", files$processed[f]))
    }
    
    processed_too_big <- processed %>%
      filter(detection_type == "harbor_seal" | detection_type == "harbor_pup") %>%
      mutate(width = bound_right - bound_left,
             height = bound_bottom - bound_top) %>%
      filter(height > 150 | width > 150)
    
    if (nrow(processed_too_big) > 0) {
      stop(paste0("Bounding boxes too large for harbor seals in ", files$processed[f]))
    }
    
    processed_too_small <- processed %>%
      filter(detection_type == "suppressed" | detection_type == "reviewed") %>%
      mutate(width = bound_right - bound_left,
             height = bound_bottom - bound_top) %>%
      filter(height <= 150 | width <= 150)
    
    if (nrow(processed_too_big) > 0) {
      stop(paste0("Bounding boxes too small for reviewed/suppressed zones in ", files$processed[f]))
    }
    
    processed_seals <- processed %>%
      filter(detection_type != "reviewed") 
    
    if (nrow(processed_seals) > 0) {
      processed_seals <- processed_seals %>%
        mutate(image_name = basename(sapply(strsplit(image_name, split= "\\/"), function(x) x[length(x)]))) %>%
        mutate(id = 1:n() + processed_id$max) %>%
        mutate(detection_file = files$processed[f]) %>%
        mutate(flight = str_extract(image_name, "fl[0-9][0-9]")) %>%
        mutate(camera_view = gsub("_", "", str_extract(image_name, "_[A-Z]_"))) %>%
        mutate(detection_id = paste(survey_id, survey_year, str_extract(image_name, "fl[0-9][0-9]"), gsub("_", "", str_extract(image_name, "_[A-Z]_")), detection, sep = "_")) %>%
        select("id", "detection", "image_name", "frame_number", "bound_left", "bound_top", "bound_right", "bound_bottom", "score", "length", "detection_type", "type_score", 
               "flight", "camera_view", "detection_id", "detection_file", "poly_cocoa")
    
      # Import data to DB
      RPostgreSQL::dbWriteTable(con, c("surv_pv_gla", "tbl_detections_processed_rgb"), processed_seals, append = TRUE, row.names = FALSE)
    }
  }
}

imported_files <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT detection_file FROM surv_pv_gla.tbl_detections_processed_rgb ORDER BY detection_file")

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con)
