library(tidyverse)
library(tidymodels)
library(glmnet)
library(randomForest)
library(xgboost)
library(Metrics)     
library(furrr)
library(tune)
library(plotly)
library(scales)

plan(multisession, workers = parallel::detectCores())

# Load and preprocess data
df <- read_csv("Final_Dataset_Modified.csv")

# Define cleaning function for monetary values
clean_money <- function(x) {
  if(is.character(x)) {
    x <- x %>%
      str_remove_all("\\$|,") %>%
      str_replace_all("—|-", "0")
    
    # Handle B/M/K notation consistently (from first file)
    if (any(str_detect(x, "[BMK]"))) {
      x <- sapply(x, function(val) {
        if (is.na(val) || val == "") return(0)
        
        if (str_detect(val, "B")) {
          return(as.numeric(str_remove(val, "B")) * 1e9)
        } else if (str_detect(val, "M")) {
          return(as.numeric(str_remove(val, "M")) * 1e6)
        } else if (str_detect(val, "K")) {
          return(as.numeric(str_remove(val, "K")) * 1e3)
        } else {
          return(as.numeric(val))
        }
      })
    } else {
      x[x == ""] <- "0"
      x <- suppressWarnings(as.numeric(x))
    }
    
    return(x)
  } else if(is.numeric(x)) {
    return(x)
  } else {
    return(rep(0, length(x)))
  }
}

# Function to format numbers with B, M, K notation for display
format_currency <- function(x) {
  if (is.na(x)) return(NA_real_) 
  
  if (x >= 1e9) {
    return(round(x / 1e9, 1))
  } else if (x >= 1e6) {
    return(round(x / 1e6, 1))
  } else if (x >= 1e3) {
    return(round(x / 1e3, 1))
  } else {
    return(round(x, 1))
  }
}

# Function to clean investor names
clean_investor_names <- function(investor_string) {
  if (is.na(investor_string) || investor_string == "") {
    return(character(0))
  }
  
  investors <- str_split(investor_string, ",")[[1]] %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("\\.$", "") %>%
    str_replace_all("(?i)\\bllc\\b", "LLC") %>%
    str_replace_all("(?i)\\blp\\b", "LP") %>%
    str_replace_all("(?i)\\bllp\\b", "LLP") %>%
    str_replace_all("(?i)\\binc\\.?\\b", "Inc") %>%
    str_replace_all("(?i)\\bventures?\\b", "Ventures") %>%
    str_replace_all("(?i)\\bcapital\\b", "Capital")
  
  # Remove empty or single-character entries
  investors <- investors[nchar(investors) > 1]
  return(paste(unique(investors), collapse = ", "))
}

# Function to clean industry names
clean_industry_names <- function(industry) {
  if (is.na(industry) || industry == "") {
    return(NA)
  }
  
  # Convert to title case for consistency
  industry <- str_to_title(industry)
  
  # Standardize common variations
  industry <- case_when(
    str_detect(industry, "(?i)fintech|finttech") ~ "Fintech", # Fixed typo from first file
    str_detect(industry, "(?i)artificial intelligence|ai") ~ "Artificial Intelligence",
    str_detect(industry, "(?i)enterprise software") ~ "Enterprise Software",
    str_detect(industry, "(?i)healthcare|health tech") ~ "Healthcare",
    str_detect(industry, "(?i)e-commerce|ecommerce") ~ "E-commerce",
    str_detect(industry, "(?i)cybersecurity") ~ "Cybersecurity",
    str_detect(industry, "(?i)cloud computing") ~ "Cloud Computing",
    str_detect(industry, "(?i)blockchain") ~ "Blockchain",
    str_detect(industry, "(?i)edtech|education") ~ "EdTech",
    str_detect(industry, "(?i)clean energy") ~ "Clean Energy",
    str_detect(industry, "(?i)robotics") ~ "Robotics",
    str_detect(industry, "(?i)digital media") ~ "Digital Media",
    str_detect(industry, "(?i)marketplace") ~ "Marketplace",
    str_detect(industry, "(?i)mobile") ~ "Mobile",
    str_detect(industry, "(?i)saas") ~ "SaaS",
    str_detect(industry, "(?i)consumer|retail|shopping|fashion|beauty|food|delivery|consumer & retail|consumer and retail") ~ "Consumer & Retail",
    TRUE ~ industry
  )
  
  return(industry)
}

# Function to clean financial stage
clean_financial_stage <- function(stage) {
  if (is.na(stage) || stage == "") {
    return(NA)
  }
  
  # Convert to title case for consistency
  stage <- str_to_title(stage)
  
  # Standardize acquisition-related stages
  stage <- case_when(
    str_detect(stage, "(?i)acq|acquired|acquisition") ~ "Acquired",
    TRUE ~ stage
  )
  
  return(stage)
}

# Modify the preprocess_data function to include industry and financial stage cleaning
preprocess_data <- function(df) {
  # Extract year from 'Date Joined' (from first file)
  df <- df %>% 
    mutate(`Date Joined` = sapply(strsplit(`Date Joined`, "/"), function(x) tail(x, 1)))
  
  df_clean <- df %>%
    select_if(~!all(is.na(.))) %>%  # Remove columns that are all NA
    filter(!if_all(everything(), is.na))  # Remove rows that are all NA
  
  # Handle the numeric conversions
  df_clean <- df_clean %>%
    mutate(
      `Valuation ($B)` = clean_money(`Valuation ($B)`),
      `Total Raised` = clean_money(`Total Raised`),
      `Estimated Revenue` = clean_money(`Estimated Revenue`),
      `Founded Year` = as.numeric(as.character(`Founded Year`)),
      `Investors Count` = as.numeric(as.character(`Investors Count`)),
      `Number of Employees` = as.numeric(as.character(`Number of Employees`)),
      `Deal Terms` = as.numeric(as.character(`Deal Terms`)),
      `Profitability Status` = ifelse(`Profitability Status` == "Yes", 1, 0) # From first file
    ) %>%
    # Replace remaining NAs in numeric columns with 0
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    filter(!is.na(`Valuation ($B)`), `Valuation ($B)` > 0)
  
  # Clean investor data
  df_clean <- df_clean %>%
    mutate(`Select Investors` = sapply(`Select Investors`, clean_investor_names))
  
  # Clean industry and financial stage data
  df_clean <- df_clean %>%
    mutate(
      Industry = sapply(Industry, clean_industry_names),
      `Financial Stage` = sapply(`Financial Stage`, clean_financial_stage)
    )
  
  # Fix industry naming issues (from first file)
  df_clean$Industry <- str_replace(df_clean$Industry, "Finttech", "Fintech")
  df_clean$Industry <- str_replace(df_clean$Industry, "Artificial intelligence", "Artificial Intelligence")
  
  # Fix industry-investor mismatch (from first file) - adjusted to work with the cleaned data
  investor_lists <- c(
    "Sequoia Capital, Thoma Bravo, Softbank",
    "Tiger Global Management, Tiger Brokers, DCM Ventures",
    "Jungle Ventures, Accel, Venture Highway",
    "Vision Plus Capital, GSR Ventures, ZhenFund",
    "Hopu Investment Management, Boyu Capital, DC Thomson Ventures",
    "500 Global, Rakuten Ventures, Golden Gate Ventures",
    "Sequoia Capital China, ING, Alibaba Entrepreneurs Fund",
    "Sequoia Capital China, Shunwei Capital Partners, Qualgro",
    "Dragonfly Captial, Qiming Venture Partners, DST Global",
    "SingTel Innov8, Alpha JWC Ventures, Golden Gate Ventures",
    "Mundi Ventures, Doqling Capital Partners, Activant Capital",
    "Vertex Ventures SE Asia, Global Founders Capital, Visa Ventures",
    "Andreessen Horowitz, DST Global, IDG Capital",
    "B Capital Group, Monk's Hill Ventures, Dynamic Parcel Distribution",
    "Temasek, Guggenheim Investments, Qatar Investment Authority",
    "Kuang-Chi"
  )
  
  # Check if Industry contains investor lists and replace with City if so
  df_clean <- df_clean %>%
    mutate(Industry = ifelse(Industry %in% investor_lists, City, Industry))
  
  # Remove industries with 1 or fewer companies
  industry_counts <- table(df_clean$Industry)
  valid_industries <- names(industry_counts[industry_counts > 1])
  df_clean <- df_clean %>%
    filter(Industry %in% valid_industries)
  
  # Add derived metrics for the dashboard (including Years in Business from first file)
  df_clean <- df_clean %>%
    mutate(
      `Years in Business` = as.numeric(format(Sys.Date(), "%Y")) - `Founded Year`,
      `Company Age` = `Years in Business`, # For consistency with second file
      `Revenue per Employee` = `Estimated Revenue` * 1000 / pmax(`Number of Employees`, 1),
      `Fundraising Efficiency` = `Valuation ($B)` / pmax(`Total Raised`, 0.01),
      `Capital Efficiency` = `Estimated Revenue` / pmax(`Total Raised`, 0.01)
    )
  
  return(df_clean)
}

# Process the data
df_clean <- preprocess_data(df)

# Print industry statistics before and after cleaning
print("\nIndustry Statistics:")
print("Before cleaning:")
print(table(df$Industry))
print(paste("Number of unique industries before cleaning:", length(unique(df$Industry))))

print("\nAfter cleaning:")
print(table(df_clean$Industry))
print(paste("Number of unique industries after cleaning:", length(unique(df_clean$Industry))))

# Add this after df_clean creation to get investor statistics
investor_stats <- df_clean %>%
  filter(!is.na(`Select Investors`), `Select Investors` != "") %>%
  separate_rows(`Select Investors`, sep = ", ") %>%
  summarise(
    Total_Investor_Mentions = n(),
    Unique_Investors = n_distinct(`Select Investors`)
  )

# Print investor statistics
print("Investor Statistics:")
print(paste("Total investor mentions:", investor_stats$Total_Investor_Mentions))
print(paste("Unique investors after cleaning:", investor_stats$Unique_Investors))

# Create investor summary for the dashboard
investor_summary <- df_clean %>%
  filter(!is.na(`Select Investors`), `Select Investors` != "") %>%
  separate_rows(`Select Investors`, sep = ", ") %>%
  group_by(`Select Investors`) %>%
  summarise(
    Companies = n(),
    Total_Valuation = sum(`Valuation ($B)`, na.rm = TRUE),
    Avg_Valuation = mean(`Valuation ($B)`, na.rm = TRUE),
    Industries = n_distinct(Industry),
    .groups = 'drop'
  ) %>%
  arrange(desc(Companies))

# Create investor features
investor_features <- investor_summary %>%
  mutate(
    Investor_Tier = case_when(
      Companies >= 10 ~ "Major",
      Companies >= 5 ~ "Active",
      TRUE ~ "Regular"
    ),
    Avg_Portfolio_Value = Total_Valuation / Companies,
    Industry_Diversity = Industries / Companies
  )

# Add investor features to main dataset
df_clean <- df_clean %>%
  mutate(
    Has_Major_Investor = map_lgl(`Select Investors`, function(invs) {
      if (length(invs) == 0 || is.na(invs) || invs == "") {
        return(FALSE)
      }
      major_investors <- investor_features %>% 
        filter(Investor_Tier == "Major") %>% 
        pull(`Select Investors`)
      any(str_split(invs, ", ")[[1]] %in% major_investors)
    }),
    
    Avg_Investor_Experience = map_dbl(`Select Investors`, function(invs) {
      if (length(invs) == 0 || is.na(invs) || invs == "") {
        return(0)
      }
      investors <- str_split(invs, ", ")[[1]]
      experience <- investor_features$Companies[investor_features$`Select Investors` %in% investors]
      if (length(experience) == 0) {
        return(0)
      }
      mean(experience, na.rm = TRUE)
    }),
    
    Avg_Investor_Portfolio_Value = map_dbl(`Select Investors`, function(invs) {
      if (length(invs) == 0 || is.na(invs) || invs == "") {
        return(0)
      }
      investors <- str_split(invs, ", ")[[1]]
      portfolio_values <- investor_features$Avg_Portfolio_Value[investor_features$`Select Investors` %in% investors]
      if (length(portfolio_values) == 0) {
        return(0)
      }
      mean(portfolio_values, na.rm = TRUE)
    }),
    
    Investor_Industry_Diversity = map_dbl(`Select Investors`, function(invs) {
      if (length(invs) == 0 || is.na(invs) || invs == "") {
        return(0)
      }
      investors <- str_split(invs, ", ")[[1]]
      diversity_scores <- investor_features$Industry_Diversity[investor_features$`Select Investors` %in% investors]
      if (length(diversity_scores) == 0) {
        return(0)
      }
      mean(diversity_scores, na.rm = TRUE)
    })
  )

# Add error checking for the new columns
print("Checking new investor features:")
print(paste("NA values in Has_Major_Investor:", sum(is.na(df_clean$Has_Major_Investor))))
print(paste("NA values in Avg_Investor_Experience:", sum(is.na(df_clean$Avg_Investor_Experience))))
print(paste("NA values in Avg_Investor_Portfolio_Value:", sum(is.na(df_clean$Avg_Investor_Portfolio_Value))))
print(paste("NA values in Investor_Industry_Diversity:", sum(is.na(df_clean$Investor_Industry_Diversity))))

# Print summary of investor features
print("\nSummary of investor features:")
print(summary(df_clean[, c("Has_Major_Investor", "Avg_Investor_Experience", 
                           "Avg_Investor_Portfolio_Value", "Investor_Industry_Diversity")]))

# Extract unique levels from the data before splitting
# This ensures our test data won't have novel factor levels
unique_countries <- unique(df_clean$Country)
unique_industries <- unique(df_clean$Industry)

# Train-test split & Feature Engineering - Using a proper validation strategy to prevent overfitting
set.seed(42) # Consistent seed for reproducibility

# Use a more robust validation strategy (from both files)
data_split <- initial_split(df_clean, prop = 0.7, strata = "Valuation ($B)")
train_data <- training(data_split)
test_data <- testing(data_split)

# Update the feature recipe to include important elements from both files
feature_recipe <- recipe(`Valuation ($B)` ~ ., data = train_data) %>%
  # Remove columns that aren't needed for prediction
  step_rm(Company, `Date Joined`, `Select Investors`, `Financial Stage`, 
          `Deal Terms`, `Portfolio Exits`, `Unicorn Status Year`, 
          `Revenue per Employee`, `Fundraising Efficiency`,
          `Capital Efficiency`, `Company Age`) %>%
  # Add role for new investor features
  step_mutate(
    Has_Major_Investor = as.factor(Has_Major_Investor),
    `Profitability Status` = as.factor(`Profitability Status`)
  ) %>%
  # Impute any remaining missing values
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # Handle unseen categories
  step_novel(all_nominal_predictors(), new_level = "other") %>%
  # Convert categorical variables to dummy variables
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Add interaction terms between key features
  step_interact(~ `Estimated Revenue`:`Total Raised`) %>%
  step_interact(~ `Avg_Investor_Experience`:`Avg_Investor_Portfolio_Value`) %>%
  # Add polynomial terms for key numeric predictors (with limited degree to avoid overfitting)
  step_poly(`Founded Year`, degree = 2) %>%
  step_poly(`Avg_Investor_Experience`, degree = 2) %>%
  # Remove zero variance predictors BEFORE normalizing
  step_zv(all_predictors()) %>%
  # Normalize all numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Remove highly correlated features (using a proper threshold to avoid multicollinearity)
  step_corr(all_numeric_predictors(), threshold = 0.9, use = "pairwise.complete.obs")

# Prep the recipe
prepped_recipe <- prep(feature_recipe)

# Bake the data
train_processed <- bake(prepped_recipe, new_data = train_data)
test_processed <- bake(prepped_recipe, new_data = test_data)

# Define features and target for model training
X_train <- train_processed %>% select(-`Valuation ($B)`)
y_train <- train_processed$`Valuation ($B)`
X_test <- test_processed %>% select(-`Valuation ($B)`)
y_test <- test_processed$`Valuation ($B)`

# Model Training & Hyperparameter Tuning
# Define models with regularization to prevent overfitting
models <- list(
  "elastic_net" = linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet"),
  
  "random_forest" = rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("regression"),
  
  "xgboost" = boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    mtry = tune(),
    sample_size = tune() # Adding subsampling to prevent overfitting
  ) %>%
    set_engine("xgboost") %>%
    set_mode("regression")
)

# Calculate number of predictors in processed data for mtry range
num_predictors <- ncol(X_train)

# Set up grid search parameters with focus on preventing overfitting
grid_params <- list(
  "elastic_net" = grid_regular(
    penalty(range = c(1e-4, 10), trans = log10_trans()), 
    mixture(range = c(0, 1)), 
    levels = 10 # Reduced from 20 to be more efficient
  ),
  "random_forest" = grid_regular(
    mtry(range = c(2, floor(sqrt(num_predictors)))), # Rule of thumb for regression
    min_n(range = c(5, 20)),
    levels = 5 # Reduced from 10 to be more efficient
  ),
  "xgboost" = grid_regular(
    trees(range = c(50, 300)),
    tree_depth(range = c(3, 6)), # Limited depth to prevent overfitting
    learn_rate(range = c(0.01, 0.1), trans = log10_trans()),
    loss_reduction(range = c(1, 10)), # Increased from 0 to enforce more regularization
    mtry(range = c(2, floor(num_predictors * 0.5))), # Limited feature fraction
    levels = 3 # Reduced from 5 to be more efficient
  )
)

# Cross-validation with more folds for better error estimates
cv_folds <- vfold_cv(train_data, v = 5, strata = "Valuation ($B)")

# Add early stopping for XGBoost from the first file
xgb_params <- list(
  early_stopping_rounds = 10,
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Train and tune models
tuned_models <- list()
best_model <- NULL

# Function to safely tune models with error handling
safely_tune_model <- function(model_name, model_spec, grid, feature_recipe, cv_folds) {
  cat("\nTuning model:", model_name, "\n")
  
  # Create workflow
  tune_workflow <- workflow() %>%
    add_recipe(feature_recipe) %>%
    add_model(model_spec)
  
  # Try to tune the model with error handling
  tryCatch({
    # Add resampling control to save predictions and use parallel processing
    ctrl <- control_grid(
      verbose = TRUE, 
      save_pred = TRUE,
      parallel_over = "resamples" # Enable parallel processing
    )
    
    tune_res <- tune_grid(
      tune_workflow,
      resamples = cv_folds,
      grid = grid,
      metrics = metric_set(rmse, rsq, mae), # Added more metrics from first file
      control = ctrl
    )
    
    # Get best parameters and fit model
    best_params <- select_best(tune_res, metric = "rmse")
    cat("Best parameters for", model_name, ":", paste(names(best_params), best_params, sep = "=", collapse = ", "), "\n")
    
    best_workflow <- finalize_workflow(tune_workflow, best_params) %>%
      fit(train_data)
    
    return(list(tuned = tune_res, best = best_workflow))
  }, error = function(e) {
    cat("Error tuning", model_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Loop through models and tune them
for (model_name in names(models)) {
  model_spec <- models[[model_name]]
  grid <- grid_params[[model_name]]
  
  result <- safely_tune_model(model_name, model_spec, grid, feature_recipe, cv_folds)
  
  if (!is.null(result)) {
    tuned_models[[model_name]] <- result$tuned
    if (is.null(best_model)) {
      best_model <- result$best
      best_model_name <- model_name
    }
  }
}

# Define an XGBoost model directly to ensure compatibility with first file approach
# This serves as a fallback if tidymodels tuning fails
xgb_direct <- function() {
  # Create DMatrix objects
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  # Parameters based on common best practices
  params <- list(
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 5,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train model with early stopping
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  return(xgb_model)
}

# Train direct XGBoost model
xgb_model_direct <- xgb_direct()

# Model Evaluation - Comprehensive approach combining both files
# Enhanced model evaluation function to include multiple metrics
evaluate_model <- function(model, test_data, model_name = NULL, is_direct_xgb = FALSE) {
  # Check if model exists
  if (is.null(model)) {
    return(list(rmse = Inf, rsq = 0, mae = Inf))
  }
  
  # Make predictions
  if (is_direct_xgb) {
    predictions <- predict(model, as.matrix(X_test))
    pred_df <- data.frame(
      actual = y_test, 
      predicted = predictions
    )
  } else {
    predictions <- predict(model, test_data)
    pred_df <- data.frame(
      actual = test_data$`Valuation ($B)`, 
      predicted = predictions$.pred
    )
  }
  
  # Calculate metrics from both files
  rmse_value <- rmse(pred_df$actual, pred_df$predicted)
  mae_value <- mae(pred_df$actual, pred_df$predicted)
  r2_value <- cor(pred_df$predicted, pred_df$actual)^2 # Calculate R² manually
  mse_value <- mse(pred_df$actual, pred_df$predicted)
  
  # For tidymodels compatibility
  if (!is_direct_xgb) {
    rsq_result <- rsq(
      pred_df,
      truth = actual,
      estimate = predicted
    )
    rsq_value <- rsq_result$.estimate
  } else {
    rsq_value <- r2_value
  }
  
  # Print results in format from first file
  if (!is.null(model_name)) {
    cat(sprintf("\n📊 %s Performance:\n", model_name))
    cat(sprintf("MSE: %.2e\n", mse_value))
    cat(sprintf("RMSE: %.2e\n", rmse_value))
    cat(sprintf("MAE: %.2e\n", mae_value))
    cat(sprintf("R² Score: %.4f\n", r2_value))
  }
  
  return(list(
    rmse = rmse_value, 
    rsq = rsq_value, 
    r2 = r2_value,
    mae = mae_value,
    mse = mse_value,
    pred_df = pred_df
  ))
}

# Define valid_models
valid_models <- names(tuned_models)[!sapply(tuned_models, is.null)]

# Update the model evaluation section
model_results <- list()

# Evaluate valid tidymodels models
if (length(valid_models) > 0) {
  for (model_name in valid_models) {
    model_results[[model_name]] <- evaluate_model(
      tuned_models[[model_name]]$best, 
      test_data, 
      model_name
    )
  }
} else {
  cat("\n⚠️ Warning: No tidymodels-based models trained successfully.\n")
}

# Always evaluate fallback xgboost_direct
model_results[["xgboost_direct"]] <- evaluate_model(
  xgb_model_direct, 
  test_data, 
  "XGBoost Direct", 
  is_direct_xgb = TRUE
)

# Extract performance metrics
model_rmse <- sapply(model_results, function(x) x$rmse)
model_rsq <- sapply(model_results, function(x) x$rsq)
model_mae <- sapply(model_results, function(x) x$mae)

# Safely determine best model
if (length(model_rmse) > 0 && any(!is.na(model_rmse))) {
  best_model_name <- names(which.min(model_rmse))
  
  if (best_model_name == "xgboost_direct") {
    best_model <- xgb_model_direct
  } else {
    best_model <- tuned_models[[best_model_name]]$best
  }
} else {
  cat("\n⚠️ No valid models found. Defaulting to direct XGBoost.\n")
  best_model_name <- "xgboost_direct"
  best_model <- xgb_model_direct
}

# Output performance comparison
cat("\n=== Model Comparison ===\n")
performance_df <- data.frame(
  Model = names(model_results),
  RMSE = model_rmse,
  MAE = model_mae,
  R2 = sapply(model_results, function(x) x$r2)
)
print(performance_df)

# Extract variable importance from the best model if available
var_importance <- NULL
if (best_model_name == "random_forest") {
  tryCatch({
    model_fit <- best_model$fit$fit
    if (!inherits(model_fit, "ranger")) {
      model_fit <- model_fit$fit  # For nested model objects
    }
    if (model_fit$importance.mode != "none") {
      importance_vals <- ranger::importance(model_fit)
      var_importance <- data.frame(
        Feature = names(importance_vals),
        Importance = importance_vals
      ) %>% arrange(desc(Importance))
    }
  }, error = function(e) {
    var_importance <- NULL
  })
} else if (best_model_name == "xgboost" || best_model_name == "xgboost_direct") {
  tryCatch({
    if (best_model_name == "xgboost") {
      xgb_model <- extract_fit_engine(best_model)
    } else {
      xgb_model <- best_model
    }
    importance_matrix <- xgb.importance(model = xgb_model)
    var_importance <- importance_matrix %>%
      select(Feature, Importance = Gain) %>%
      arrange(desc(Importance))
  }, error = function(e) {
    var_importance <- NULL
  })
} else if (best_model_name == "elastic_net") {
  tryCatch({
    glmnet_model <- extract_fit_engine(best_model)
    best_lambda <- best_model$fit$spec$args$penalty
    if (is.null(best_lambda)) {
      best_lambda <- glmnet_model$lambda.min
    }
    coefs <- coef(glmnet_model, s = best_lambda)
    var_importance <- data.frame(
      Feature = rownames(coefs),
      Importance = abs(as.vector(coefs))
    ) %>% 
      filter(Importance != 0) %>%
      arrange(desc(Importance))
  }, error = function(e) {
    var_importance <- NULL
  })
}

# Show variable importance
if (!is.null(var_importance)) {
  cat("\n=== Top 20 Important Features ===\n")
  print(head(var_importance, 20))
}

# Prepare data for dashboard visualizations
top_countries <- df_clean %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    `Total Valuation` = sum(`Valuation ($B)`),
    `Avg Valuation` = mean(`Valuation ($B)`)
  ) %>%
  arrange(desc(Count)) %>%
  head(10)

# Top Industries
top_industries <- df_clean %>%
  group_by(Industry) %>%
  summarise(
    Count = n(),
    `Total Valuation` = sum(`Valuation ($B)`),
    `Avg Valuation` = mean(`Valuation ($B)`)
  ) %>%
  arrange(desc(Count)) %>%
  head(10)

# Valuation vs. Total Raised
val_vs_raised <- df_clean %>%
  select(`Valuation ($B)`, `Total Raised`, Industry) %>%
  head(200) # Limit points for better visualization

# Valuation vs. Revenue
val_vs_revenue <- df_clean %>%
  select(`Valuation ($B)`, `Estimated Revenue`, Industry) %>%
  head(200) # Limit points for better visualization

# Company Age vs. Valuation
age_vs_val <- df_clean %>%
  select(`Valuation ($B)`, `Company Age`, Industry) %>%
  head(200) # Limit points for better visualization

# Employee Count vs. Valuation
employees_vs_val <- df_clean %>%
  select(`Valuation ($B)`, `Number of Employees`, Industry) %>%
  head(200) # Limit points for better visualization

# Time trends for founding years
founding_trends <- df_clean %>%
  group_by(`Founded Year`) %>%
  summarise(
    `Count` = n(),
    `Avg Valuation` = mean(`Valuation ($B)`)
  ) %>%
  filter(`Founded Year` >= 1990, `Founded Year` <= as.numeric(format(Sys.Date(), "%Y")))

# Safety check for model variables
if (!exists("best_model_name") || is.null(best_model_name)) {
  cat("Warning: No best model was selected. Setting default values.\n")
  best_model_name <- "none"
  best_model <- NULL
  model_rsq <- list()
  var_importance <- data.frame(Feature = character(), Importance = numeric())
}

df_clean <- df_clean %>%
  mutate(
    `Valuation ($B)` = sapply(`Valuation ($B)`, format_currency),
    `Total Raised` = sapply(`Total Raised`, format_currency),
    `Estimated Revenue` = sapply(`Estimated Revenue`, format_currency)
  )

top_countries <- top_countries %>%
  mutate(
    `Total Valuation` = sapply(`Total Valuation`, format_currency),
    `Avg Valuation` = sapply(`Avg Valuation`, format_currency)
  )

top_industries <- top_industries %>%
  mutate(
    `Total Valuation` = sapply(`Total Valuation`, format_currency),
    `Avg Valuation` = sapply(`Avg Valuation`, format_currency)
  )

val_vs_raised <- val_vs_raised %>%
  mutate(
    `Valuation ($B)` = sapply(`Valuation ($B)`, format_currency),
    `Total Raised` = sapply(`Total Raised`, format_currency)
  )

val_vs_revenue <- val_vs_revenue %>%
  mutate(
    `Valuation ($B)` = sapply(`Valuation ($B)`, format_currency),
    `Estimated Revenue` = sapply(`Estimated Revenue`, format_currency)
  )

founding_trends <- founding_trends %>%
  mutate(`Avg Valuation` = sapply(`Avg Valuation`, format_currency))

# Save all objects needed for the app
save(df_clean, best_model, best_model_name, model_rsq, var_importance, 
     top_countries, top_industries, val_vs_raised, val_vs_revenue, 
     founding_trends, unique_countries, unique_industries, model_results,
     investor_summary, investor_stats, investor_features,
     file = "unicorn_data.RData")