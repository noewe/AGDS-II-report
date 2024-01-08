# Loads the rasters of all the covariates used in the waterlog.100 model 
# of the Digital Soil Mapping report

source("./R/check_pkg.R")
pkgs <- c("dplyr", "tidyverse", "caret", "ggplot2", "terra", "ranger")
check_pkg(pkgs)


# Load random forest model
rf_prob   <- readRDS(here::here("data/rf_prob_waterlog100.rds"))
rf_bor   <- readRDS(here::here("data/rf_bor_waterlog100.rds"))

# Load area to be predicted
raster_mask <- terra::rast(here::here("data-raw/geodata/study_area/area_to_be_mapped.tif"))

# Turn target raster into a dataframe, 1 px = 1 cell
df_mask <- as.data.frame(raster_mask, xy = TRUE)

# Filter only for area of interest
# Since we only want to predict on a given study area, the TIF file comes with a labeling of 0 for pixels that are outside the area of interest and 1 for pixels within the area of interest.
df_mask <- df_mask |> 
  dplyr::filter(area_to_be_mapped == 1)

# Display df
head(df_mask) |> 
  knitr::kable()


# Covariates rasters-------------------------------------------------------
# Get a list of all available covariate file names.
files_covariates <- list.files(
  path = here::here("data-raw/geodata/covariates/"), 
  pattern = ".tif$",
  recursive = TRUE, 
  full.names = TRUE
)

# Load the rasters for the selected predictor variables into a raster object (a “stack” of multiple rasters).
# Filter that list only for the variables used in the RF model
preds_selected <- rf_prob$forest$independent.variable.names
files_selected <- files_covariates[apply(sapply(X = preds_selected, 
                                                FUN = grepl, 
                                                files_covariates), 
                                         MARGIN =  1, 
                                         FUN = any)]

# Load all rasters as a stack
raster_covariates <- terra::rast(files_selected)


# Convert the raster stack into a dataframe - the preferred format for model prediction.
# Get coordinates for which we want data
df_locations <- df_mask |> 
  dplyr::select(x, y)

# Extract data from covariate raster stack for all gridcells in the raster
df_predict <- terra::extract(
  raster_covariates,   # The raster we want to extract from
  df_locations,        # A matrix of x and y values to extract for
  ID = FALSE           # To not add a default ID column to the output
)

df_predict <- cbind(df_locations, df_predict) |> 
  tidyr::drop_na()  # Se_TWI2m has a small number of missing data


# Make predictions map: probability --------------------------------------------
## Make probability predictions using the RF model 
prediction <- predict(
  rf_prob,              # RF model
  data = df_predict,   
  num.threads = parallel::detectCores() - 1)

# Attach predictions to dataframe and round them
pred_prob_full <- prediction$predictions
pred_prob_full <- as.vector(pred_prob_full[,2])
df_predict$pred_prob <- as.factor(pred_prob_full)

# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, pred_prob)

# Turn dataframe into a raster
raster_pred <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = raster_pred) +
  ggplot2::scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "probability"
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted probability of soil waterlogging (100 cm)")

# Write soil PH raster to a GeoTIFF file

# Save raster as .tif file
terra::writeRaster(
  raster_pred,
  "data/predicted_waterlog100_prob.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)

# Make predictions map: Absolute Predictions -----------------------------------

## Make binary redictions using the RF model 
prediction <- predict(
  rf_bor,              # RF model
  data = df_predict,   
  num.threads = parallel::detectCores() - 1)

# Attach predictions to dataframe and round them
pred_bor_full <- prediction$predictions
df_predict$pred_bor <- as.factor(pred_bor_full)

# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, pred_bor)

# Turn dataframe into a raster
raster_pred_bin <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_bin) +
  ggplot2::scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "waterlog (0 or 1)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted soil waterlogging (100 cm)")

# Write soil PH raster to a GeoTIFF file

# Save raster as .tif file
terra::writeRaster(
  raster_pred_bin,
  "data/predicted_waterlog100_binary.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)

# Thresholds -------------------------------------------------------------------
# Create binary rasters from the probability predictions, using different thresholds

df_predict <- df_predict |>
  mutate(pred_prob_num = pred_prob_full) |>
  mutate(pred_critical = as.factor(ifelse(pred_prob_num > 0.01, 1, 0)), # choose a 1% probability as threshold
         pred_unwanted = as.factor(ifelse(pred_prob_num > 0.1, 1, 0)), # choose a 10% probability as threshold
         pred_unwanted2 = as.factor(ifelse(pred_prob_num > 0.05, 1, 0))) 


# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, pred_unwanted)

# Turn dataframe into a raster
raster_pred_unwanted <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_unwanted) +
  ggplot2::scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "waterlog (0 or 1)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted soil waterlogging (100 cm)")

# Save raster as .tif file
terra::writeRaster(
  raster_pred_unwanted,
  "data/predicted_waterlog100_10perc.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)

# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, pred_unwanted2)

# Turn dataframe into a raster
raster_pred_unwanted2 <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_unwanted2) +
  ggplot2::scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "waterlog (0 or 1)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted soil waterlogging (100 cm)")

# Save raster as .tif file
terra::writeRaster(
  raster_pred_unwanted,
  "data/predicted_waterlog100_10perc.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)

# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, pred_critical)

# Turn dataframe into a raster
raster_pred_critical <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_critical) +
  ggplot2::scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "waterlog (0 or 1)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted soil waterlogging (100 cm)")

# Save raster as .tif file
terra::writeRaster(
  raster_pred_critical,
  "data/predicted_waterlog100_1perc.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)
