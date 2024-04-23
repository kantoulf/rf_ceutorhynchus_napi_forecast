
f_parallel <- function(x = 7){
  plan(multisession, workers = x)
}

f_class <- function(dataframe){
  map_dfr(dataframe, class)
}

f_vc <- function(annee, data){
  train_set <- data %>% dplyr::filter(Campagne != annee)
  test_set <- data %>% dplyr::filter(Campagne == annee)
  train_set <- train_set %>% dplyr::select(-Campagne)
  test_set <- test_set %>% dplyr::select(-Campagne)
  rf <- ranger::ranger(
    Capture ~ ., 
    data = train_set, 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    mtry = 4, 
    min.node.size = 10,
    probability = TRUE,
    save.memory = FALSE,
    num.threads = 15
  )
  
  predictions <- predict(rf, test_set) %>% predictions()
  
  res <- data.frame(
    data = test_set$Capture, 
    proba = predictions[,"Pres"]
  ) %>% 
    mutate(Campagne = annee)
  
  return(list(rf, res))
}

f_vc_ROC <- function(dataset){
  annees <- unique(dataset$Campagne)
  vc_annees <- purrr::map(
    .x = annees, 
    data = dataset,
    .f = f_vc
  )
  vc_bind <- do.call(rbind, lapply(X = vc_annees, "[[", 2))
  models <- lapply(X = vc_annees, "[[", 1)
  ROC <- pROC::roc(
    response = vc_bind$data, 
    predictor = vc_bind$proba,
    levels = c("Abs", "Pres")
  )
  return(list(vc_bind, ROC, models))
}

f_roc <- function(annees, donnees){
  data <- donnees %>% 
    filter(Campagne %in% annees)
  roc <- roc(
    response = data$data, 
    predictor = data$proba,
    levels = c("Abs", "Pres")
  )
  return(roc$auc)
}

f_plotImp <- function(rf){
  rf$variable.importance %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    set_colnames(c("variable", "importance")) %>% 
    ggplot(aes(x = reorder(variable, -importance), y = importance)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

f_predict <- function(annee, data, list_models){
  pred <- predict(list_models[[as.character(annee)]], data %>% filter(Campagne == annee))
  pred$predictions[, "Pres"]
}

f_flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

f_filter_var <- function(data, df_imp, seuil){
  all_var <<- names(data %>% select(-Capture, -Campagne))
  mcor <<- rcorr(as.matrix(data %>% select(-Capture, -Campagne)))
  df_cor <<- f_flattenCorrMatrix(mcor$r, mcor$P) %>% 
    arrange(row) %>% 
    filter(abs(cor) >= seuil) %>%
    rename(var1 = row, var2 = column) %>% 
    arrange(desc(cor))
  while(nrow(df_cor) > 0){
    var1 <- df_cor %>% slice_head() %>% pull(var1) %>% as.character()
    var2 <- df_cor %>% slice_head() %>% pull(var2) %>% as.character() 
    var1_imp <- df_imp %>% filter(variable == var1) %>% pull(valeur)
    var2_imp <- df_imp %>% filter(variable == var2) %>% pull(valeur)
    var_to_remove <- case_when(var1_imp > var2_imp ~ var2,
                               var2_imp > var1_imp ~ var1)
    df_cor <<- df_cor %>% filter(!str_detect(var1, var_to_remove),
                                 !str_detect(var2, var_to_remove))
    all_var <<- all_var[all_var != var_to_remove]
  }
  c("Capture", "Campagne", all_var)
}

f_yhat <- function(X.model, newdata) {
  predict(X.model, newdata) %>% 
    predictions() %>% 
    as.data.frame() %>% 
    pull(Pres)
}

f_best_threshold <- function(roc_vc){
  data_roc <- roc_vc[[1]]
  ROC_all <- roc(
    response = data_roc$data, 
    predictor = data_roc$proba,
    levels = c("Abs", "Pres")
  )
  coords(
    roc = ROC_all, 
    x = "best", 
    input = "threshold"
  ) 
}

f_unscale <- function(datanorm){t(t(datanorm) * scale + center)}

f_smote <- function(annee){
  a <- scaled_predata %>% slice(index_campagne[[as.character(annee)]])
  b <- bimba::SMOTE(a, perc_min = 50)
  f_unscale(b %>% select(-Capture) %>% as.matrix) %>% bind_cols(Capture = b$Capture)
}

f_rf <- function(annee, data){
  train_set <- data %>% filter(Campagne != annee)
  train_set <- train_set %>% dplyr::select(-Campagne)
  ranger(
    Capture ~ ., 
    data = train_set, 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    mtry = 4, 
    min.node.size = 10,
    probability = TRUE,
    save.memory = FALSE
  )
}

f_doy <- function(colname, data){
  df1 <- data %>% 
    group_by(Id_plot) %>% 
    filter(get(colname) == "Pres") %>% 
    slice_head() %>% 
    select(Id_plot, Date_obs) %>% 
    set_colnames(c("Id_plot", paste0("date_", colname))) %>% 
    ungroup()
  return(left_join(unique_idplot, df1, by = "Id_plot") %>% ungroup() %>%  select(-Id_plot))
}

f_rmse <- function(vecEcarts){
  sqrt(sum(vecEcarts^2, na.rm = T)/length(vecEcarts))
}