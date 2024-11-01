# Load required packages --------------------------------------------------
library(tidymodels)
library(readr)

# Source other scripts ----------------------------------------------------
upload <- new.env(); source("./R/upload.R", local = upload)

run_mnreg_GHQ <- function(){
  # Load in data
  df <- upload$hse18lab

  # Set GHQ12Scr as the variable of interest
  df$GHQ12Scr = as.factor(df$GHQ12Scr)

  # Clean data to remove NAs
  df_clean <- df %>% filter(!is.na(GHQ12Scr))

  # Run a test plot of BMI status against GHQ12Scr
  df_cleanBMI <- df_clean %>% filter(!is.na(BMIvg5), BMIOK == 1)

  p <- ggplot(df_cleanBMI, aes(BMIvg5, fill = GHQ12Scr)) +
    geom_bar() +
    coord_flip()
  print(p)

  # Clean all out
  df1 <- df[, !names(df) %in% "BMIOK"]
  df2 <- df1[, !names(df1) %in% "ILL12m"]
  df3 <- df2[, !names(df2) %in% "IllAff7"]
  df_extraclean <- na.omit(df3)
  #print(summary(df_extraclean))

  # Split data into training and test datasets
  set.seed(679)
  split <- initial_split(df_extraclean, prop=0.8, strata = GHQ12Scr)
  train <- split %>% training()
  test <- split %>% testing()
  #print("here")
  #print(summary(train$GHQ12Scr))

  # Train a logistic regression model
  model <- multinom_reg(mixture = double(1), penalty = double(1)) %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    fit(GHQ12Scr ~ ., data = train)

  # Model summary
  print(tidy(model))

  # Class Predictions
  pred_class <- predict(model,
                        new_data = test,
                        type = "class")

  # Class Probabilities
  pred_proba <- predict(model,
                        new_data = test,
                        type = "prob")

  results <- test %>%
    select(GHQ12Scr) %>%
    bind_cols(pred_class, pred_proba)

  accuracy(results, truth = GHQ12Scr, estimate = .pred_class)

  # Hyperparameter tuning -------------------------------------------------------
  # Define the logistic regression model with penalty and mixture hyperparameters
  mn_reg <- multinom_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

  # Define the grid search for the hyperparameters
  grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

  # Define the workflow for the model
  mn_reg_wf <- workflow() %>%
    add_model(mn_reg) %>%
    add_formula(GHQ12Scr ~ .)

  # Define the resampling method for the grid search
  folds <- vfold_cv(train, v = 5)

  # Tune the hyperparameters using the grid search
  mn_reg_tuned <- tune_grid(
    mn_reg_wf,
    resamples = folds,
    grid = grid,
    control = control_grid(save_pred = TRUE)
  )

  print(select_best(mn_reg_tuned, metric = "roc_auc"))

  # Fit the model using the optimal hyperparameters
  mn_reg_final <- multinom_reg(penalty = 0.0000000001, mixture = 0) %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    fit(GHQ12Scr~., data = train)

  # Evaluate the model performance on the testing set
  pred_class <- predict(mn_reg_final,
                        new_data = test,
                        type = "class")
  results <- test %>%
    select(GHQ12Scr) %>%
    bind_cols(pred_class, pred_proba)

  # Create confusion matrix
  conf_mat(results, truth = GHQ12Scr,
           estimate = .pred_class)

  # calculate the precision (positive predictive value, the number of true positives divided by the number of predicted positives)
  precision(results, truth = GHQ12Scr,
            estimate = .pred_class)

  # calculate the recall (sensitivity, the number of true positives divided by the number of actual positives)
  recall(results, truth = GHQ12Scr,
         estimate = .pred_class)

  # higher the value of coefficients the higher their importance is
  coeff <- tidy(mn_reg_final) %>%
    arrange(desc(abs(estimate))) %>%
    filter(abs(estimate) > 0.5)

  # Plot importance
  ggplot(coeff, aes(x = term, y = estimate, fill = term)) + geom_col() + coord_flip()

}
