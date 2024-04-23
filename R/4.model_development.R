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

# l'idee ici est de faire un RF "naif" avec toutes les donnees et voir les perfs obtenues.

ROC_test <- f_vc_ROC(data_ranger)

saveRDS(ROC_test, "data/export/rds/ROC_test.rds")

data_roc_full <- ROC_test[[1]]
models_full <- ROC_test[[3]]
names(models_full) <- 2011:2020

# AUC par annee de 2011 à 2022
map_dbl(
  .x = unique(data_ranger$Campagne), 
  donnees = ROC_test[[1]], 
  .f = f_roc
)

# ROC sur l'ensemble des predictions
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

# matrice de confusion
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

# broom::tidy(confmat_full) %>% 
#   list(as.data.frame(confmat_full$table)) %>% 
#   writexl::write_xlsx("data/export/article_ocl/confmat_full.xlsx")

# mesure de l'importance des variables

# l'idee ici est de repliquer plusieurs RF pour s'affranchir de la variation 
# inherente aux RF dans l'estimation de l'importance des variables

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

# meme operation que dans le paragrpahe precedent, mais avec seulement les 
# variables retenues par caret dans le script "3.Selection_variables.R"

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

# j'aurais pu faire des fonctions pour toutes ces etapes que je repete plusieurs fois...
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

# On filtre les variables selon leur importance et leur correlations
tokeep <- f_filter_var(data_ranger_caret, df_imp_caret, 0.75)

data_ranger_minimal <- data_ranger_caret %>% 
  select(all_of(tokeep))

saveRDS(data_ranger_minimal, "data/export/rds/data_ranger_minimal.RDS")


# RF filtered caret + correlations ----------------------------------------

# A ce stade, on a cree un jeu de donnes avec uniquement des variables importantes 
# et faiblement correlees entre elles.
# On peut generer un dernier modele "minimal" qui ne comprend que ces variables.

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

# ranger final utilisable pour predictions futures
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

# ALEPLOTS. Idem Partial Dependant Plot mais prend mieux en compte le probleme des 
# variables correlees. Voir ici : https://christophm.github.io/interpretable-ml-book/ale.html
predictor_test <- Predictor$new(
  model = rf_min,
  data = data_ranger_minimal %>% select(-Campagne),
  predict.function = f_yhat,
  y = "Capture",
  # class = "Pres",
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

# meme modele, mais avec latitude/longitude/jour conserves.
# Ces variables avaient etes ecartees lors de la phase de selection.
# Malgre tout, elles ne sont pas de nature meteorologiques comme les autres.
# Elles pourraient quand meme permettre de preciser les predictions dans certains cas.

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