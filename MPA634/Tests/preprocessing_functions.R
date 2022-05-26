remove_empty <- function(df, cutoff = 1) {
  df <- df[rowSums(is.na(df)) != ncol(df),]
  df <- df %>% select_if(!colSums(is.na(df)) > nrow(df)/cutoff)
  return(df)
}

lump_factors <- function(df, label, prop = .05, remove_label = TRUE) {
  if (remove_label) {
    df <- df %>% select(-{{label}}) %>% mutate(across(where(is.factor), ~fct_lump_prop(.x, prop = prop)))
  } else {
    df <- df %>% mutate(across(where(is.factor), ~fct_lump_prop(.x, prop = prop)))
  }
  return(df)
}

make_dummies <- function(df) {
  df <- df %>% dummy_cols(remove_first_dummy = TRUE, 
                          ignore_na = TRUE,
                          remove_selected_columns = TRUE)
  return(df)
}

impute <- function(df, method = "knn"){
  if(method == "knn"){
    df <- predict(preProcess(as.data.frame(df), method = "knnImpute"), as.data.frame(df))
  } else if (method == "mean"){
    df <- df %>% mutate(across(where(is.numeric), ~replace_na(.x, mean(.x, na.rm = TRUE))))
  } else if (method == "median"){
    df <- df %>% mutate(across(where(is.numeric), ~replace_na(.x, median(.x, na.rm = TRUE))))
  } else {
    stop("Please pick a valid method: 'knn', 'mean', or 'median'.")
  }
}

remove_near_zero <- function(df, print_results = FALSE) {
  nzv <- nearZeroVar(df)
  if(is.null(nzv) | length(nzv) == 0) {
    if(print_results) print("Near Zero Variables: None")
  } else {
    df <- df[,-nzv]
    if(print_results) {
      print("Near Zero Variables: ") 
      print(nzv)
    }
  }
  return(df)
}

remove_linear_combos <- function(df, print_results = FALSE){
  combos <- findLinearCombos(df)
  if(is.null(combos$remove)) {
    if(print_results) print(paste0("Linear Combos: None")) 
  } else {
    if(print_results) print(combos)
    df <- df[-combos$remove]
  }
  return(df)
}

remove_correlated <- function(df, print_results = FALSE, cutoff = .75) {
  highlyCorDescr <- findCorrelation(cor(df), cutoff = cutoff)
  if(length(highlyCorDescr) == 0){
    if(print_results) print("Highly Correlated Variables: None")
  } else {
    df <- df[,-highlyCorDescr]
    if(print_results) {
      print("Highly Correlated Variables: ")
      print(highlyCorDescr)
    }
  }
  return(df)
}

scale_data <- function(df){
  df <- df %>% scale() %>% as_tibble()
  return(df)
}

replace_label <- function(df_cleaned, df_original, label){
  df <- cbind(df_original %>% select({{label}}), df_cleaned) %>% na.omit()
  return(df)
}

classification_train <- function(df, label, method = "lda") {
  start_time <- proc.time()[3]
  model <- train(as.formula(paste(label, "~.", collapse="")), df, method = method)
  if (model[["modelType"]] == "Classification") {
    accuracy <- max(model[["results"]][["Accuracy"]])
    print(paste0("Model: ", method, "    Accuracy: ", round(accuracy, 2), "   Time: ", round(proc.time()[3] - start_time, 2)))
  } else {
    rmse <- min(model[["results"]][["RMSE"]])
    mae <- min(model[["results"]][["MAE"]])
    r2 <- max(model[["results"]][["Rsquared"]])
    print(paste0("Model: ", method, "    RMSE: ", round(rmse, 2),"    MAE: ", round(mae, 2),"    R-squared: ", round(r2, 2), "   Time: ", round(proc.time()[3] - start_time, 2)))
  }
  return(model)
}