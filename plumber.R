# Fitting the best model and defining the API

# libraries
library(tidyverse)
library(tidymodels)
library(plumber)
set.seed(42)

#--------------Fit the model--------------

# read in data
diabetes <- read_csv(file = "diabetes_binary_health_indicators_BRFSS2015.csv")

# clean up data
diabetes <- diabetes |>
  mutate(Diabetes_binary = as.factor(Diabetes_binary),
         HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol),
         CholCheck = as.factor(CholCheck),
         Smoker = as.factor(Smoker),
         Stroke = as.factor(Stroke),
         HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
         PhysActivity = as.factor(PhysActivity),
         Fruits = as.factor(Fruits),
         Veggies = as.factor(Veggies),
         HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
         AnyHealthcare = as.factor(AnyHealthcare),
         NoDocbcCost = as.factor(NoDocbcCost),
         DiffWalk = as.factor(DiffWalk),
         GenHlth = factor(GenHlth,
                          levels = 1:5,
                          labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
         Sex = factor(Sex, levels = 0:1, labels = c("Female", "Male")),
         Age = factor(Age,
                      levels = 1:13,
                      labels = c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                                 "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", 
                                 "70 to 74", "75 to 79", "80 and over")),
         Education = factor(Education,
                            levels = 1:6,
                            labels = c("Never attended/Only Kindergarten", "Elementary", 
                                       "Some high school", "High school graduate", 
                                       "Some college/tech school", "College graduate")),
         Income = factor(Income,
                         levels = 1:8,
                         labels = c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999", 
                                    "$20,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999",
                                    "$50,000 to $74,999", "$75,000 or greater"))
  )

# find averages/most prevalent value for predictors in model
default_values <- diabetes |>
  select(GenHlth, HighBP, HighChol, DiffWalk, BMI, PhysHlth) |>
  summarize(across(where(is.numeric),
                   ~ round(mean(.x), digits = 2),
                   .names = "{col}"),
            across(c("HighBP", "HighChol", "DiffWalk"),
                   ~ sum(as.numeric(as.character(.x)))/n(),
                   .names = "{col}"))

# GenHlth is categorical (but not binary), so need to choose the most prevalent value
GenHlth_val <- diabetes |>
  group_by(GenHlth) |>
  summarize(count = n()) |>
  arrange(desc(count))

# add that value to default values
default_values["GenHlth"] <- GenHlth_val[1, 1]

# for categorical variables, convert value to 1 if >= 0.5, 0 otherwise
default_values <- default_values |>
  mutate(across(c("HighBP", "HighChol", "DiffWalk"),
                ~ factor(if_else(.x >= 0.5, 1, 0),
                         levels = c(0, 1)),
                .names = "{col}"))


# read in model fitted on the full data (see EDA.html for how this model was fit)
rf_fit_model <- readRDS("rf_fit_model.rda")

#------------API Setup----------------------

#* @apiTitle Random Forest Model API
#* @apiDescription Run random forest model for specified parameters, plot 

#------ Endpoints ------

# 1) pred endpoint - take in 6 predictors, with default values

#* Return a prediction for diabetes status based on 6 predictors
#* @param BMI BMI
#* @param PhysHlth Days of illness or injury within last 30 days (numeric 0-30)
#* @param HighBP High Blood Pressure (0 = false or 1 = true) 
#* @param HighChol High Cholesterol (0 = false or 1 = true) 
#* @param DiffWalk Difficulty Walking (0 = false or 1 = true) 
#* @param GenHlth Rate general health (1 = excellent to 5 = poor)
#* @get /pred
function(BMI = 28.4, PhysHlth = 4.24, HighBP = 0, HighChol = 0, DiffWalk = 0, GenHlth = "2") {
  
  # create tibble for values
  test_values <- tibble(
    BMI = as.numeric(BMI),
    PhysHlth = as.numeric(PhysHlth),
    HighBP = factor(HighBP, levels = c("0", "1")),
    HighChol = factor(HighChol, levels = c("0", "1")),
    DiffWalk = factor(DiffWalk, levels = c("0", "1")),
    GenHlth = factor(GenHlth, 
                     levels = 1:5,
                     labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))
  )
  
  # run model
  rf_fit_model |>
    predict(new_data = test_values)

}

# example function calls using the pred endpoint
# 1) http://127.0.0.1:9651/pred
# 2) http://127.0.0.1:9651/pred?BMI=35&PhysHlth=12&HighBP=1&HighChol=1&DiffWalk=1&GenHlth=4
# 3) http://127.0.0.1:9651/pred?BMI=28&HighBP=1


# 2) info - return name and github page URL

#* Info for the site
#* @get /info
function() {
  list("Katy Kearns", "github URL")
}

# 3) confusion - return a plot of the confusion matrix

#* Plot confusion matrix
#* @serializer png
#* @get /confusion
function() {
  
  # read in confusion matrix that has already been created (see api.R file)
  rf_conf_mat <- readRDS(file = "rf_conf_matrix.rda")
  
  # create plot of the confusion matrix
  print(autoplot(rf_conf_mat, type = "mosaic"))
           
}
