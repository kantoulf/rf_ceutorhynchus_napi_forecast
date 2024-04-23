# Packages ----------------------------------------------------------------
{
  library(ranger)
  library(VSURF)
  library(Boruta)
  library(caret)
  library(varSelRF)
  library(MLmetrics)
  library(pROC)
  library(tidyverse)
}

# Data --------------------------------------------------------------------
bsv <- readRDS("data/export/rds/bsv_mai2022.rds")

data_vsurf <- bsv %>% 
  select(-c(Id_plot:Campagne))

data_vsurf_cr <- scale(data_vsurf)


# VSURF -------------------------------------------------------------------
vsurf_res <- VSURF(
  x = data_vsurf %>% select(-Capture) %>% as.matrix(),
  y = data_vsurf %>% pull(Capture),
  RFimplem = "ranger",
  probability = TRUE,
  parallel = TRUE,
  ncores = 7
)


# Boruta ------------------------------------------------------------------
boruta_res <- Boruta(
  x = data_vsurf %>% select(-Capture) %>% as.matrix(),
  y = data_vsurf %>% pull(Capture),
  maxRuns = 500
)


# varSelRF ----------------------------------------------------------------
varSelRF_res <- varSelRF(
  xdata = data_vsurf %>% select(-Capture) %>% as.matrix(), 
  Class = data_vsurf %>% pull(Capture), 
  c.sd = 1, 
  mtryFactor = 1, 
  ntree = 5000,
  ntreeIterat = 2000, 
  vars.drop.num = NULL, 
  vars.drop.frac = 0.2,
  whole.range = TRUE, 
  recompute.var.imp = FALSE, 
  verbose = TRUE,
  returnFirstForest = TRUE, 
  fitted.rf = NULL, 
  keep.forest = FALSE
)

saveRDS(varSelRF_res, "varSelRF_res.rds")


# Caret -------------------------------------------------------------------
data_caret <- bsv %>% 
  select(-c(Id_plot:Valeur)) %>% 
  select(Campagne : sTmax_0) %>% 
  mutate(
    Capture = case_when(Capture == 0 ~ "Abs", TRUE ~ "Pres"),
    Capture = factor(Capture, levels = c("Pres", "Abs"))
  ) %>% 
  drop_na()

index <- lapply(unique(data_caret$Campagne),function(i) which(data_caret$Campagne != i))

data_caret <- data_caret %>% select(-Campagne)

tgrid <- expand.grid(
  mtry = 2:4,
  splitrule = c("extratrees"),
  min.node.size = c(10, 20, 50)
)

rangerFuncs <- list(
  fit = function(x, y, first, last, ...) {
    train(
      x, 
      y,
      method = "ranger",
      importance = "permutation",
      metric = "ROC",
      tuneGrid = tgrid,
      trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary),
      num.threads = 7,
      ...
    )
  },
  pred = function(object, x) {
    out <- data.frame(predict(object, newdata = x, type="prob"), pred = predict(object, newdata = x))
    return(out)
  },
  rank = function(object, x, y) {
    varImp(object)[["importance"]] %>% rownames_to_column() %>% arrange(desc(Overall)) %>% rename(var = rowname)
  },
  selectSize = pickSizeBest,
  selectVar = pickVars,
  summary = twoClassSummary
)

control <- rfeControl(
  functions = rangerFuncs,
  rerank = FALSE,
  method = "cv",
  index = index,
  number = NA,
  verbose = TRUE,
  returnResamp = "final",
  allowParallel = TRUE)

subsets <- c(4) 
set.seed(42)

RfeProfile <- rfe(
  Capture ~ ., 
  data = data_caret,
  sizes = subsets, 
  metric = "ROC", 
  maximize = TRUE, 
  rfeControl = control
)
