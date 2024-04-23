# Packages ----------------------------------------------------------------
{
  library(pdp)
  library(magrittr)
  library(lubridate)
  library(furrr)
  library(data.table)
  library(ranger)
  library(pROC)
  library(magrittr)
  library(Hmisc)
  library(corrplot)
  library(RANN)
  library(ALEPlot)
  library(iml)
  library(caret)
  library(RcppRoll)
  library(sf)
  library(rmapshaper)
  library(ConfusionTableR)
  library(gridExtra)
  library(tidyverse)
}

test %>% 
  group_by(Id_plot) %>% 
  mutate(delay = Date_obs - lag(Date_obs))

# Data --------------------------------------------------------------------
{
  bsv <- readRDS("data/export/rds/bsv_mai2022.rds") %>% 
    arrange(Id_plot, Date_obs)
  
  bsv_etendu <- readRDS("data/export/rds/bsv_etendu_mai2022.rds") %>% 
    arrange(Id_plot, Date_obs)
  
  bsv_etendu_minimal <- readRDS("data/bsv_etendu_minimal_jourLatLong.rds")
  
  data_ranger <- bsv %>% 
    arrange(Id_plot, Date_obs) %>% 
    select(-c(Id_plot : Valeur, lat_nearest, long_nearest, id_safran, idunique)) %>% 
    drop_na()
  
  data_ranger_minimal <- readRDS("data/export/rds/data_ranger_minimal.RDS")
  
  rfe <- readRDS("data/export/rds/rfeProfile.rds")
}

# Fonctions ---------------------------------------------------------------
source(file = "R/Fonctions.R")

# RF full dataset ---------------------------------------------------------

ROC_test <- f_vc_ROC(data_ranger)

saveRDS(ROC_test, "data/export/rds/ROC_test.rds")

data_roc_full <- ROC_test[[1]]
models_full <- ROC_test[[3]]
names(models_full) <- 2011:2020

map_dbl(
  .x = unique(data_ranger$Campagne), 
  donnees = ROC_test[[1]], 
  .f = f_roc
)

ROC_all <- roc(
  response = data_roc_full$data, 
  predictor = data_roc_full$proba,
  levels = c("Abs", "Pres")
)

best_seuil <- coords(
  roc = ROC_all, 
  x = "best", 
  input = "threshold",
  best.method = "closest.topleft"
) %>% 
  pull(threshold)

df_matrice <- data_roc_full %>% 
  mutate(
    pred = ifelse(proba >= best_seuil, "Pres", "Abs"),
    pred = factor(pred, levels = c("Pres", "Abs"))
  )

confmat_full <- confusionMatrix(
  data = df_matrice$pred,
  reference = df_matrice$data,
  positive = "Pres"
)

rf_replicate <- replicate(
  20, 
  ranger(
    Capture ~ ., 
    data = data_ranger %>% select(-Campagne), 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    min.node.size = 10,
    mtry = 4,
    probability = TRUE
  ),
  simplify = FALSE
)

rf_imps <- do.call(rbind, lapply(X = rf_replicate, "[[", 6)) %>% 
  as.data.frame()

df_imp_full <- tibble(
  valeur = apply(rf_imps, 2, mean),
  sd = apply(rf_imps, 2, sd),
  variable = names(rf_imps)
) %>% 
  arrange(desc(valeur)) 

df_imp_full %>% 
  ggplot()+
  geom_col(aes(x = reorder(variable, -valeur), y = valeur)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# RF filtered caret -------------------------------------------------------

var_imp <- rfe$variables %>% 
  group_by(var) %>% 
  summarise(importance = mean(Overall)) %>% 
  arrange(-importance)

tokeep <- c(rfe$optVariables)

data_ranger_caret <- bsv %>% 
  select(-c(Id_plot : Valeur)) %>% 
  select(Campagne, Capture, all_of(tokeep))

ROC_caret <- f_vc_ROC(dataset = data_ranger_caret)

map(
  .x = unique(data_ranger_caret$Campagne), 
  donnees = ROC_caret[[1]], 
  .f = f_roc
)

rf_replicate_caret <- replicate(
  20, 
  ranger(
    Capture ~ ., 
    data = data_ranger_caret %>% select(-Campagne), 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    mtry = 4, 
    min.node.size = 10,
    probability = TRUE
  ),
  simplify = FALSE
)

rf_imps_caret <- do.call(rbind, lapply(X = rf_replicate_caret, "[[", 6)) %>% 
  as.data.frame()

df_imp_caret <- tibble(
  valeur = apply(rf_imps_caret, 2, mean),
  sd = apply(rf_imps_caret, 2, sd),
  variable = names(rf_imps_caret)
) %>% 
  arrange(desc(valeur)) 

df_imp_caret %>% 
  ggplot()+
  geom_col(aes(x = reorder(variable, -valeur), y = valeur)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

tokeep <- f_filter_var(data_ranger_caret, df_imp_caret, 0.75)

data_ranger_minimal <- data_ranger_caret %>% 
  select(all_of(tokeep))

saveRDS(data_ranger_minimal, "data/export/rds/data_ranger_minimal.RDS")


# RF filtered caret + correlations ----------------------------------------

ROC_minimal <- f_vc_ROC(dataset = data_ranger_minimal)
saveRDS(ROC_minimal, "data/export/rds/ROC_minimal.rds")

data_roc_minimal <- ROC_minimal[[1]]
models_minimal <- ROC_minimal[[3]]
names(models_minimal) <- 2011:2022

map_dbl(
  unique(data_ranger_minimal$Campagne), 
  donnees = ROC_minimal[[1]], 
  f_roc
)

rf_replicate_min <- replicate(
  20, 
  ranger(
    Capture ~ ., 
    data = data_ranger_minimal %>% select(-Campagne), 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    mtry = 4, 
    min.node.size = 10,
    probability = TRUE
  ),
  simplify = FALSE
)

rf_imps_min <- do.call(rbind, lapply(X = rf_replicate_min, "[[", 6)) %>% 
  as.data.frame()

df_imp_min <- tibble(
  valeur = apply(rf_imps_min, 2, mean),
  sd = apply(rf_imps_min, 2, sd),
  variable = names(rf_imps_min)
) %>% 
  arrange(desc(valeur)) 

rf_min <- ranger(
  Capture ~ ., 
  data = data_ranger_minimal %>% select(-Campagne), 
  num.trees = 500, 
  importance = "permutation", 
  splitrule = "extratrees",
  mtry = 4, 
  min.node.size = 10,
  probability = TRUE
)

f_plotImp(rf_min)

seuil_minimal <- f_best_threshold(ROC_minimal) %>% pull(threshold)

predictor_test <- Predictor$new(
  model = rf_min,
  data = data_ranger_minimal %>% select(-Campagne),
  predict.function = f_yhat,
  y = "Capture",
  type = "prob",
)

to_plot <- rf_min$variable.importance %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  set_colnames(c("variable", "importance")) %>% 
  pull(variable)

FeatureEffects$new(
  predictor = predictor_test,
  features = to_plot,
  grid.size = 50
) %>% 
  plot(rug = F)


# RF pour shiny ---------------------------------------------------------

data_shiny <- read_csv("data/import/data_shiny.csv") %>% 
  mutate(Capture = factor(Capture, levels = c("Pres", "Abs")))

roc_shiny <- f_vc_ROC(data_shiny) 

map_dbl(
  .x = 2011:2022, 
  donnees = roc_shiny[[1]], 
  .f = f_roc
)

f_best_threshold(roc_shiny)

rf_final <- ranger(
  Capture ~ ., 
  data = data_shiny %>% select(-Campagne), 
  num.trees = 500, 
  importance = "permutation", 
  splitrule = "extratrees",
  mtry = 4, 
  min.node.size = 10,
  probability = TRUE
)

saveRDS(rf_final, "data/export/rds/rf_final.rds")
save(rf_final, file = "data/rds/rdata/rf_final.RData")