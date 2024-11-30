# libraries
library(tidyverse)
library(tidymodels)
library(plumber)
set.seed(42)

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
# 
# # recast factors as characters
# default_values <- default_values |>
#   mutate(across(where(is.factor),
#                 as.character,
#                 .names = "{col}"))


#------- Model

# stuff for training the model
diabetes_split <- initial_split(diabetes, prop = 0.7)
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)
diabetes_CV_folds <- vfold_cv(diabetes_train, 5)

# model specifications (using the optimal mtry value found during tuning)
rf_model <- rand_forest(mtry = 4) |>
  set_engine("ranger") |>
  set_mode("classification")

# recipe
tree_rec <- recipe(Diabetes_binary ~ GenHlth + HighBP + HighChol + DiffWalk + BMI + PhysHlth,
                   data = diabetes_train) |>
  step_normalize(all_numeric()) |>
  step_dummy(GenHlth, HighBP, HighChol, DiffWalk)

# workflow 
rf_wkf <- workflow() |>
  add_recipe(tree_rec) |>
  add_model(rf_model)

# fit best model on entire data set
rf_fit_full <- rf_wkf |>
  fit(diabetes)

#------ Endpoints ------

# 1) pred endpoint - take in 6 predictors, with default values

#* Return a prediction for diabetes status based on 6 predictors
#* @param pred1 BMI
#* @param pred2 PhysHlth
#* @param pred3 HighBP
#* @param pred4 HighChol
#* @param pred5 DiffWalk 
#* @param pred6 GenHlth

function(pred1 = 28.4, pred2 = 4.24, pred3 = 0, pred4 = 0, pred5 = 0, pred6 = "Very Good") {
  
}


  # convert character fields to numeric where necessary

# # test
# default_values[1,] <- list(35, 5, "1", "1", "0", "Poor")


# predict - need to put the input values (or default values) in as the new_data
rf_fit_full |>
  predict(new_data = default_values)



# example function calls using the pred endpoint
# 1) 
# 2)
# 3)

# info


# confusion

