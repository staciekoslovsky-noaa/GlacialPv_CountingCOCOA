# Glacial Pv Surveys: Export image list, annotation file, and shapefile of selected footprints for counting

# STARTING VARIABLES 
survey_id <- 'margerie_20240817_targetedcount_1' # survey_id to be counted
interval2keep <- 1 # keep every nth image for image review (this needs to be evaluated in QGIS before running this code)

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
install_pkg("sf")

# Run code -------------------------------------------------------
# Set initial working directory 
wd <- paste("C:/smk", survey_id, sep = "/")

if (file.exists(wd) == TRUE) {
  unlink(wd, recursive = TRUE)
}

dir.create(wd)
setwd(wd)

# Get data from the DB for processing
con <-   RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = Sys.getenv("pep_db"),
                        host = Sys.getenv("pep_ip"),
                        user = Sys.getenv("pep_user"),
                        password = Sys.getenv("user_pw"))

images_all <- sf::st_read(con, query = paste0("SELECT * FROM surv_pv_gla.geo_images_footprint_network_path WHERE survey_id = \'", survey_id, "\' ORDER BY image_name"),
                          geometry_column = "geom") %>%
  mutate(dt = str_extract(image_name, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9][0-9]"))

# Subset images to every nth frame
images_selected_C <- images_all %>%
  filter(camera_view == "C") 

if (interval2keep > 1) {
  images_selected_C <- images_selected_C %>%
    filter(row_number() %% interval2keep == 1)
}


# Create footprints for selected images
footprints <- images_all %>%
  filter(dt %in% images_selected_C$dt)

# Export image lists for C, L, R camera views
images_selected_C <- images_selected_C %>%
  select(image_path) %>%
  st_drop_geometry()
write.table(images_selected_C, paste0(survey_id, "_C_images_", format(Sys.time(), "%Y%m%d"), ".txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

images_selected_L <- footprints %>%
  filter(camera_view == "L") %>%
  select(image_path) %>%
  st_drop_geometry()
write.table(images_selected_L, paste0(survey_id, "_L_images_", format(Sys.time(), "%Y%m%d"), ".txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

images_selected_R <- footprints %>%
  filter(camera_view == "R") %>%
  select(image_path) %>%
  st_drop_geometry()
write.table(images_selected_R, paste0(survey_id, "_R_images_", format(Sys.time(), "%Y%m%d"), ".txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

RPostgreSQL::dbDisconnect(con)
rm(con)