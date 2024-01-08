###########################################
# Model training for digital soil mapping #
###########################################

source("./R/check_pkg.R")
pkgs <- c("Boruta", "dplyr", "tidyverse", "ranger", "caret")
check_pkg(pkgs)


# Preprocessing ----------------------------------------------------------------

# Load the data
df_full <- readRDS(here::here("data/df_full.rds")) |>
  mutate(waterlog.30 = as.factor(waterlog.30),
         waterlog.50 = as.factor(waterlog.50),
         waterlog.100 = as.factor(waterlog.100))


# Specify target: The pH in the top 10cm
target <- "waterlog.100"

# Specify predictors_all: Remove soil sampling and observational data
predictors_all <- names(df_full)[14:ncol(df_full)]

cat("The target is:", target,
    "\nThe predictors_all are:", paste0(predictors_all[1:8], sep = ", "), "...")


# Split dataset into training and testing sets
df_train <- df_full |> dplyr::filter(dataset == "calibration")
df_test  <- df_full |> dplyr::filter(dataset == "validation")

# Filter out any NA to avoid error when running a Random Forest
df_train <- df_train |> tidyr::drop_na()
df_test <- df_test   |> tidyr::drop_na()

saveRDS(df_train,                   
        here::here("data/df_train_waterlog100.rds"))
saveRDS(df_test,                   
        here::here("data/df_test_waterlog100.rds"))

# A little bit of verbose output:
n_tot <- nrow(df_train) + nrow(df_test)

perc_cal <- (nrow(df_train) / n_tot) |> round(2) * 100
perc_val <- (nrow(df_test)  / n_tot) |> round(2) * 100

cat("For model training, we have a calibration / validation split of: ",
    perc_cal, "/", perc_val, "%")


# Simple Model -----------------------------------------------------------------
# Let's run the basic model
rf_basic <- ranger::ranger( 
  y = df_train[, target],     # target variable
  x = df_train[, predictors_all],   # Predictor variables
  importance   = "permutation", # Pick permutation to calculate variable importance
  seed = 123,                    # Specify seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

saveRDS(rf_basic,                   
        here::here("data/rf_basic_waterlog100.rds"))


# Variable selection with Boruta------------------------------------------------
set.seed(42)

# run the algorithm
bor <- Boruta::Boruta(
  y = df_train[, target], 
  x = df_train[, predictors_all],
  maxRuns = 50, # Number of iterations. Set to 30 or lower if it takes too long
  num.threads = parallel::detectCores()-1)

saveRDS(bor,                   
        here::here("data/boruta_waterlog100.rds"))

# obtain results: a data frame with all variables, ordered by their importance
df_bor <- Boruta::attStats(bor) |> 
  tibble::rownames_to_column() |> 
  dplyr::arrange(dplyr::desc(meanImp))

# get retained important variables
predictors_selected <- df_bor |> 
  dplyr::filter(decision == "Confirmed") |>
  dplyr::pull(rowname)

# re-train Random Forest model
rf_bor <- ranger::ranger( 
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# quick report and performance of trained model object
rf_bor

saveRDS(rf_bor,                   
        here::here("data/rf_bor_waterlog100.rds"))


# Hyperparameter tuning with tuneGrid-------------------------------------------

# Formulate recipe
pp <- recipes::recipe(as.formula(paste(target, " ~", paste(predictors_selected, collapse = "+"))), 
                      data = df_train) |> 
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

# Define hyperparameter tuning values
mtry_values <- c(1, 2, 4, 5, 6, 7, 8, 10)
min.node.size_values <- c(1:20)
set.seed(43)
# Train model
mod <- caret::train(
  pp, 
  data = df_train %>% 
    drop_na(), 
  method = "ranger",
  #metric = "RMSE",
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final"
  ),
  # expand grid of tunable hyperparameters
  tuneGrid = expand.grid(
    .mtry = mtry_values,             
    .min.node.size = min.node.size_values,    
    .splitrule = "gini"
  ),
  # arguments specific to "ranger" method
  # keep num.trees small for computation
  # for reproducibility set the seed value
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 124
)

plot(mod)

saveRDS(mod,                   
        here::here("data/rf_tune_try2_waterlog100.rds"))

# Save accuracy overviews of tuning runs
rf_tune_try1   <- readRDS(here::here("data/rf_tune_try1_waterlog100.rds"))
rf_tune_try2   <- readRDS(here::here("data/rf_tune_try2_waterlog100.rds"))
rf_tune_try3   <- readRDS(here::here("data/rf_tune_try3_waterlog100.rds"))
rf_tune_try4   <- readRDS(here::here("data/rf_tune_try4_waterlog100.rds"))
rf_tune_try5   <- readRDS(here::here("data/rf_tune_try5_waterlog100.rds"))
rf_tune_try6   <- readRDS(here::here("data/rf_tune_try6_waterlog100.rds"))

plot <- cowplot::plot_grid(plot(rf_tune_try1), plot(rf_tune_try2), plot(rf_tune_try3), 
                           plot(rf_tune_try4), plot(rf_tune_try5), plot(rf_tune_try6), 
                           ncol = 3, labels = c("A", "B", "C", "D", "E", "F"))
ggplot2::ggsave("hyperparam_tuning.png", plot, path="../manuscript/figures", width=16, height=10)

# Calculate the final optimized model
set.seed(43)
mod <- caret::train(
  pp, 
  data = df_train %>% 
    drop_na(), 
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final"
  ),
  tuneGrid = expand.grid(
    .mtry = 4,             
    .min.node.size = 12,    
    .splitrule = "gini"
  ),
  # arguments specific to "ranger" method
  # keep num.trees small for computation
  # for reproducibility set the seed value
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 124
)

# re-train Random Forest model
set.seed(43)
rf_opt <- ranger::ranger( 
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  mtry = 4,             
  min.node.size = 12,    
  splitrule = "gini",
  seed = 124,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

saveRDS(rf_opt,                   
        here::here("data/rf_opt_waterlog100.rds"))


# Probabilistic model-----------------------------------------------------------
# Probabilistic Random Forest model
# re-train the Random Forest after Boruta variable selection, with probability = TRUE
rf_prob <- ranger::ranger( 
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  probability = TRUE,
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

saveRDS(rf_prob,                   
        here::here("data/rf_prob_waterlog100.rds"))


