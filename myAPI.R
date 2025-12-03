# myAPI.R
# Plumber API for Diabetes Health Indicators Project
# Fits final Random Forest model on full data
# Exposes /pred, /info, /confusion endpoints


# Libraries
library(plumber)

# Cant use tidymodels, so 
library(recipes)     # recipe(), step_normalize()
library(parsnip)     # rand_forest(), set_engine(), set_mode()
library(workflows)   # workflow(), add_recipe(), add_model(), fit()

# other helpers
library(dplyr)       
library(tibble)      
library(ggplot2)     
library(yardstick)   
library(readr)       


# Load and prepare data

# Read the data 
health_data <- read_csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# Recode variables to factors with meaningful labels
health_data <- health_data |>
  mutate(
    Diabetes_binary = factor(Diabetes_binary,
                             levels = c(0, 1),
                             labels = c("NoDiabetes", "Diabetes")),
    HighBP = factor(HighBP, levels = c(0,1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0,1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0,1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0,1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0,1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0,1),
                                  labels = c("No","Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0,1), labels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c(0,1), labels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c(0,1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1),
                               labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c(0,1), labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0,1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c(1:5),
                     labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    DiffWalk = factor(DiffWalk, levels = c(0,1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(0,1), labels = c("Female", "Male")),
    Age = factor(Age, levels = c(1:13),
                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44",
                            "45-49", "50-54", "55-59", "60-64",
                            "65-69", "70-74", "75-79", "80+")),
    Education = factor(Education, levels = c(1:6),
                       labels = c("KG or No School", "Elementary", "Middle school",
                                  "High school", "College","Professional Degree")),
    Income = factor(Income, levels = c(1:8),
                    labels = c("<10K", "$10k-$15k", "$15k-$20k", "$20k-$25k",
                               "$25k-$35k", "$35k-$50k", "$50k-$75k", "$75k+"))
  )

# Ensure BMI is numeric
health_data$BMI <- as.numeric(health_data$BMI)


# Final model specification
# Use the same predictors and preprocessing as in Modeling.qmd
# Diabetes_binary ~ BMI + Smoker + HighBP + HeartDiseaseorAttack + PhysActivity + Sex

rf_recipe <- recipe(
  Diabetes_binary ~ BMI + Smoker + HighBP + HeartDiseaseorAttack + PhysActivity + Sex,
  data = health_data
) |>
  step_normalize(all_numeric(), -Diabetes_binary)


# Set mtry and min_n to the best values found in Modeling.qmd.
# Tuned values.
best_mtry  <- 3   
best_min_n <- 28

rf_spec_final <- rand_forest(
  mtry  = best_mtry,
  trees = 1000,
  min_n = best_min_n
) |>
  set_engine("ranger") |>
  set_mode("classification")

rf_wf_final <- workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_spec_final)

# Fit the final model on the FULL dataset
final_rf_model <- fit(rf_wf_final, data = health_data)


# Defaults for predictors
get_most_prevalent_class <- function(column) {
  column |>
    na.omit() |>
    table() |>
    which.max() |>
    names()
}

default_values <- list(
  HighBP              = get_most_prevalent_class(health_data$HighBP),
  BMI                 = mean(health_data$BMI, na.rm = TRUE),
  Smoker              = get_most_prevalent_class(health_data$Smoker),
  HeartDiseaseorAttack = get_most_prevalent_class(health_data$HeartDiseaseorAttack),
  PhysActivity        = get_most_prevalent_class(health_data$PhysActivity),
  Sex                 = get_most_prevalent_class(health_data$Sex)
)


# Pre compute predictions for confusion matrix

predictions <- predict(final_rf_model, health_data, type = "class") |>
  bind_cols(health_data |> select(Diabetes_binary))


# API Endpoints

#* Prediction endpoint
#* Takes the predictors used in the final model and returns class + probability.
#*
#* @param HighBP Blood pressure status ("No" or "Yes"). Default: most prevalent.
#* @param BMI Body Mass Index (numeric). Default: mean BMI.
#* @param Smoker Smoking status ("No" or "Yes"). Default: most prevalent.
#* @param HeartDiseaseorAttack Heart disease / attack history ("No" or "Yes"). Default: most prevalent.
#* @param PhysActivity Physical activity ("No" or "Yes"). Default: most prevalent.
#* @param Sex Sex ("Female" or "Male"). Default: most prevalent.
#* @get /pred
pred_endpoint <- function(
    HighBP              = default_values$HighBP,
    BMI                 = default_values$BMI,
    Smoker              = default_values$Smoker,
    HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
    PhysActivity        = default_values$PhysActivity,
    Sex                 = default_values$Sex
) {
  
  # Build one-row data frame with correct types/levels
  input_data <- tibble(
    HighBP              = factor(HighBP, levels = c("No", "Yes")),
    BMI                 = as.numeric(BMI),
    Smoker              = factor(Smoker, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No", "Yes")),
    PhysActivity        = factor(PhysActivity, levels = c("No", "Yes")),
    Sex                 = factor(Sex, levels = c("Female", "Male"))
  )
  
  # Get class and probability predictions
  class_pred <- predict(final_rf_model, input_data, type = "class") |>
    pull(.pred_class)
  
  prob_pred <- predict(final_rf_model, input_data, type = "prob")
  
  # Probability of Diabetes
  prob_diabetes <- prob_pred$.pred_Diabetes
  
  list(
    input = input_data,
    predicted_class = class_pred,
    prob_Diabetes = prob_diabetes
  )
}

# ---- Example calls for /pred 
# http://127.0.0.1:8000/pred
# http://127.0.0.1:8000/pred?BMI=32&Smoker=Yes&HighBP=Yes&HeartDiseaseorAttack=No&PhysActivity=No&Sex=Male
# http://127.0.0.1:8000/pred?BMI=24&Smoker=No&HighBP=No&HeartDiseaseorAttack=No&PhysActivity=Yes&Sex=Female

#* Info endpoint
#* Returns name and GitHub Pages URL
#* @get /info
info_endpoint <- function() {
  list(
    name = "Saurabh Gupta", 
    #github_pages_url = "https://github.com/ApexDataWorld/ST558_Final_Project/" 
    github_pages_url = "https://apexdataworld.github.io/ST558_Final_Project/"
  )
}

#* Confusion matrix endpoint (heatmap)
#* Produces a plot of confusion matrix comparing model predictions vs actual
#* @serializer png
#* @get /confusion
confusion_endpoint <- function() {
  
  cm <- predictions |>
    conf_mat(truth = Diabetes_binary, estimate = .pred_class)
  
  cm_plot <- autoplot(cm, type = "heatmap") +
    ggtitle("Confusion Matrix Heatmap")
  
  # print the plot so plumber can render it
  print(cm_plot)
}


# Run the API - for local use only
# To run from R:


if (interactive()) {
  pr <- plumb("myAPI.R")
  pr$run(
    host = "0.0.0.0",
    port = 8000,
    swagger = TRUE
  )
}

#source("myAPI.R")

