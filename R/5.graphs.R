# Packages ----------------------------------------------------------------
{
  library(magrittr)
  library(lubridate)
  library(furrr)
  library(data.table)
  library(ranger)
  library(pROC)
  library(corrplot)
  library(iml)
  library(ALEPlot)
  library(sf)
  library(ConfusionTableR)
  library(gridExtra)
  library(tidyverse)
  library(caret)
}


# Data --------------------------------------------------------------------
{
  bsv <- readRDS("data/export/rds/bsv_mai2022.rds") %>% 
    arrange(Id_plot, Date_obs)
  
  bsv_etendu <- readRDS("data/export/rds/bsv_etendu_mai2022.rds") %>% 
    arrange(Id_plot, Date_obs)
  
  # bsv_etendu_minimal <- readRDS("data/bsv_etendu_minimal_jourLatLong.rds")
  
  data_ranger <- bsv %>% 
    arrange(Id_plot, Date_obs) %>% 
    select(-c(Id_plot : Valeur, lat_nearest, long_nearest, id_safran, idunique)) %>% 
    drop_na()
  
  data_ranger_minimal <- readRDS("data/export/rds/data_ranger_minimal.RDS")
  
  rfe <- readRDS("data/export/rds/rfeProfile.rds")
  
  # fondCarte <- read_sf("D:/Mes Donnees/OneDrive - Cirad/Terres_inovia/DEPARTEMENTS/DEPARTEMENT.shp") %>% 
  #   st_transform(4326) %>%
  #   filter(!grepl("CORSE", NOM_DEPT)) %>% 
  #   ms_simplify(keep = 0.02) %>% 
  #   arrange(CODE_DEPT)
  
  fondCarte <- read_sf("/media/legros/Donnees/OneDrive_linux/Terres_inovia/DEPARTEMENTS/DEPARTEMENT.shp") %>%
    # fondCarte <- read_sf("/home/legros/OneDrive/Terres_inovia/DEPARTEMENTS/DEPARTEMENT.shp") %>%
    st_transform(4326) %>%
    filter(!grepl("CORSE", NOM_DEPT)) %>% 
    arrange(CODE_DEPT)
  
}

# Fonctions ---------------------------------------------------------------

source("R/1_Dev_modele/99.Fonctions.R")

# Figure 1 (carte des idplots) --------------------------------------------

data_fig1 <- bsv %>% 
  select(Id_plot, Campagne, Latitude2, Longitude2) %>% 
  group_by(Id_plot) %>% 
  slice_head() %>% 
  ungroup

fig1 <- ggplot()+
  geom_sf(data = fondCarte)+
  geom_point(
    data = data_fig1, 
    aes(
      x = Longitude2, 
      y = Latitude2
      # color = factor(Campagne)
    ),
    shape = 3,
    color = "black",
    size = 0.75
  )+
  labs(
    x = "Longitude", 
    y = "Latitude",
    color = "Years"
  )+
  theme(
    axis.title.x = element_text(
      margin = margin(20, 0, 0, 0),
      face = "bold"
    ),
    axis.title.y = element_text(
      margin = margin(0, 20, 0, 0),
      face = "bold"
    ),
    legend.title = element_text(
      face = "bold"
    ),
    panel.background = element_rect(
      fill = "white"
    )
  )

fig1

{
  pdf(
    file = "D:/Mes Donnees/OneDrive - Cirad/Articles/Modele_vol/Figures_pdf/Figure_1.pdf",
    height = 6,
    width = 9,
  )
  print(fig1)
  dev.off()
}


# Figure 2 (variables par ordre d'importance) -----------------------------

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
    probability = TRUE,
    num.threads = 15
  ),
  simplify = FALSE
)

rf_imps <- do.call(rbind, lapply(X = rf_replicate, "[[", 6)) %>% 
  as.data.frame()

save(rf_imps, file = "5.rf_imps.RData")
save(rf_replicate, file = "5.rf_replicate.RData")

load("5.rf_imps.RData")

df_imp_full <- tibble(
  valeur = apply(rf_imps, 2, mean),
  sd = apply(rf_imps, 2, sd),
  variable = names(rf_imps)
) %>% 
  arrange(desc(valeur)) 

short_term_variables <- c(
  "ampTmin",
  "ampTmoy",
  "ampTmax",
  "sTmoy",
  "sTmoy_6",
  "sTmax",
  "sTmax_1",
  "sTmax_3",
  "sTmax_5",
  "sTmax_7",
  "sTmax_9",
  "sTmax_12",
  "sTmax_15",
  "sTmax_17",
  "sTmax_19",
  "sPluie",
  "sInsolation",
  "tMax",
  "tMin",
  "nJours_tMax_9_int",
  "nJours_sol_3_int",
  "ETPmoy",
  "ETPmax",
  "insolationMoy",
  "insolationMax",
  "RGmoy",
  "photop",
  "pluieMoy",
  "pluieMax",
  "PETPmoy"
)

medium_term_variables <- c(
  "sPluie_hiver",
  "sTmax_hiver",
  "sSol_hiver",
  "sRGmoy_hiver",
  "sETP_hiver",
  "sPETP_hiver",
  "sTmax_7_hiver",
  "sTmax_9_hiver",
  "sTmax_12_hiver",
  "sTmax_15_hiver"
)

long_term_variables <- c(
  "sPluie_ete",
  "sTmax_ete",
  "sSol_ete",
  "sRGmoy_ete",
  "sETP_ete",
  "sPETP_ete",
  "sTmax_7_ete",
  "sTmax_9_ete",
  "sTmax_12_ete",
  "sTmax_15_ete"
)

gps_variables <- c(
  "Latitude2",
  "Longitude2"
)

landscape_variables <- c(
  "scolza",
  "sforet"
)

data_fig2 <- df_imp_full %>% 
  rowwise %>% 
  mutate(
    groupe = case_when(
      any(map_lgl(gps_variables, ~grepl(.x, variable))) ~ "GPS coordinates",
      any(map_lgl(long_term_variables, ~grepl(.x, variable))) ~ "Long term",
      any(map_lgl(medium_term_variables, ~grepl(.x, variable))) ~ "Medium term",
      any(map_lgl(short_term_variables, ~grepl(.x, variable))) ~ "Short term",
      any(map_lgl(landscape_variables, ~grepl(.x, variable))) ~ "Landscape",
      variable == "jour" ~ "Julian day",
      TRUE ~ "Short term"
    ),
  ) %>% 
  ungroup() %>% 
  mutate(
    valeur = (valeur/max(valeur)) * 100,
    sd = sd * (100 / max(df_imp_full$valeur))
  )

fig2 <- data_fig2 %>% 
  ggplot()+
  geom_col(aes(x = reorder(variable, -valeur), y = valeur, fill = groupe)) +
  scale_fill_manual(values = c("black", "#CDCD00", "#CD3700", "#7A67EE", "#00C5CD", "#90EE90"))+
  geom_errorbar(
    aes(
      x = reorder(variable, -valeur),
      y = valeur,
      ymin = valeur - sd, 
      ymax = valeur + sd
    ), 
    width = 0.4,
    position = position_dodge(0.05)
  )+
  labs(
    x = "Predictors ordered by decreasing importance",
    y = "Mean relative importance (+/- sd) \n (N = 20)",
    fill = "Group of predictors"
  )+
  scale_y_continuous(
    breaks = seq(0, 100, 5),
    limits = c(0,110),
    expand = c(0, 0)
  )+
  theme(
    axis.title.x = element_text(
      face = "bold", 
      size = 16,
      margin = margin(20,0,0,0)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 16,
      margin = margin(0,20,0,0)
    ),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.text = element_text(size = 12)
  )

fig2

pdf(
  file = "D:/Mes Donnees/Terres_inovia/Article/Figures_pdf/Figure_2.pdf",
  height = 12,
  width = 22,
)
print(fig2)
dev.off()


# Figure 3 (variables min) ------------------------------------------------

rf_replicate_min <- replicate(
  20, 
  ranger(
    Capture ~ ., 
    data = data_ranger_minimal %>% select(-Campagne), 
    num.trees = 500, 
    importance = "permutation", 
    splitrule = "extratrees",
    min.node.size = 10,
    mtry = 4,
    probability = TRUE,
    num.threads = 15
  ),
  simplify = FALSE
)

rf_imps_min <- do.call(rbind, lapply(X = rf_replicate_min, "[[", 6)) %>% 
  as.data.frame()

save(rf_imps_min, file = "5.rf_imps_min.RData")
save(rf_replicate_min, file = "5.rf_replicate_min.RData")

df_imp_min <- tibble(
  valeur = apply(rf_imps_min, 2, mean),
  sd = apply(rf_imps_min, 2, sd),
  variable = names(rf_imps_min)
) %>% 
  arrange(desc(valeur)) 


data_fig3 <- df_imp_min %>% 
  rowwise %>% 
  mutate(
    groupe = case_when(
      any(map_lgl(gps_variables, ~grepl(.x, variable))) ~ "GPS coordinates",
      any(map_lgl(long_term_variables, ~grepl(.x, variable))) ~ "Long term",
      any(map_lgl(medium_term_variables, ~grepl(.x, variable))) ~ "Medium term",
      any(map_lgl(short_term_variables, ~grepl(.x, variable))) ~ "Short term",
      any(map_lgl(landscape_variables, ~grepl(.x, variable))) ~ "Landscape",
      variable == "jour" ~ "Julian day",
      TRUE ~ "Short term"
    ),
  ) %>% 
  ungroup() %>% 
  mutate(
    valeur = (valeur/max(valeur)) * 100,
    sd = sd * (100 / max(df_imp_min$valeur))
  )

fig3 <- data_fig3 %>% 
  ggplot()+
  geom_col(aes(x = reorder(variable, -valeur), y = valeur, fill = groupe)) +
  scale_fill_manual(values = c("#00C5CD", "#90EE90"))+
  geom_errorbar(
    aes(
      x = reorder(variable, -valeur),
      y = valeur,
      ymin = valeur - sd, 
      ymax = valeur + sd
    ), 
    width = 0.4,
    position = position_dodge(0.05)
  )+
  labs(
    x = "Predictors ordered by decreasing importance",
    y = "Mean relative importance (+/- sd) \n (N = 20)",
    fill = "Group of predictors"
  )+
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0,110),
    expand = c(0, 0)
  )+
  theme(
    axis.title.x = element_text(
      face = "bold", 
      size = 16,
      margin = margin(20,0,0,0)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 16,
      margin = margin(0,20,0,0)
    ),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12,
      face = "bold"
    ),
    axis.ticks.x = element_blank(),
    legend.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.text = element_text(size = 12)
  )

fig3

pdf(
  file = "D:/Mes Donnees/Terres_inovia/Article/Figures_pdf/Figure_3.pdf",
  height = 6*1.2,
  width = 11*1.2,
)
print(fig3)
dev.off()


# Figure 4 (ALE plots) ----------------------------------------------------

rf_min <- ranger(
  Capture ~ ., 
  data = data_ranger_minimal %>% select(-Campagne), 
  num.trees = 500, 
  importance = "permutation", 
  splitrule = "extratrees",
  min.node.size = 10,
  mtry = 4,
  probability = TRUE,
  num.threads = 15
)

predictor <- Predictor$new(
  model = rf_min,
  data = data_ranger_minimal %>% dplyr::select(-Campagne),
  predict.function = f_yhat,
  y = "Capture"
)

to_plot <- names(rf_min$variable.importance)

feature_data <- FeatureEffects$new(
  predictor = predictor,
  features = to_plot[1],
  grid.size = 50
) 

plot(feature_data, rug = T)

ale_effects <- FeatureEffects$new(
  predictor = predictor,
  features = to_plot,
  grid.size = 50
)

ale_values <- ale_effects$results
ale_values <- map(ale_values, ~janitor::clean_names(.x))

plot_ale <- function(ale_dataset) {
  
  feature <- ale_dataset[1, "feature"]
  tag_value <- switch(
    feature, 
    "sTmax_9_0" = "(a)",
    "sTmoy_6_28" = "(b)",
    "nJours_tMax_9_int_0" = "(c)",
    "nJours_tMax_9_int_7" = "(d)",
    "nJours_tMax_9_int_14" = "(e)",
    "nJours_tMax_9_int_21" = "(f)",
    "nJours_tMax_9_int_28" = "(g)",
    "nJours_tMax_9_int_35" = "(h)",
    "photop_21" = "(i)",
    "RGmoy_0" = "(j)",
    "RGmoy_7" = "(k)",
    "RGmoy_28" = "(l)",
    "sRGmoy_hiver_0" = "(m)",
    "nj_qt_06" = "(n)",
    "PETPmoy_0" = "(o)"
  )
  
  ggplot(data = ale_dataset, aes(x = borders, y = value))+
    geom_line(
      color = "red",
      linewidth = 2
    )+
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 1
    )+
    labs(
      x = ale_dataset %>% 
        pull(feature) %>% 
        unique(),
      y = "",
      tag = tag_value
      # y = "Effect on capture probability"
    )+
    geom_rug(
      data = data_ranger_minimal, 
      aes(
        x = ale_dataset$feature %>% unique() %>% get(),
      ),
      sides = "b",
      inherit.aes = F
    ) +
    scale_y_continuous(
      breaks = seq(-0.10, 0.10, 0.05),
      limits = c(-0.11, 0.11)
    )+
    theme(
      axis.title.x = element_text(
        face = "bold",
        size = 14,
        margin = margin(20, 0, 35, 0)
      ),
      axis.title.y = element_text(
        face = "bold",
        size = 14,
        margin = margin(0, 20, 0, 0)
      ),
      axis.text = element_text(
        size = 12
      ),
      plot.tag = element_text(size = 16, color = "#0078d4", face = "bold"),
      plot.tag.position = c(0.25, 0.9)
    )
  
}

plot_ale(ale_values[[1]])

list_ale_plots <- map(ale_values, plot_ale)
names(list_ale_plots) <- to_plot

# fig4 <- grid.arrange(
#   grobs = list_ale_plots,
#   layout_matrix = rbind(
#     c(1 ,  2,  8,  4,  5, 14,  3),
#     c(15, 13, 12, NA,  9, NA, NA),
#     c(NA,  7, 10, NA, 11, NA, NA),
#     c(NA, NA, NA, NA,  6, NA, NA)
#   )
# )

# fig4 <- grid.arrange(
#   grobs = list_ale_plots,
#   layout_matrix = rbind(
#     c( 1, 15,  3,  5,  4, 14),
#     c( 2,  8, NA,  9, NA, NA),
#     c(13, 12, NA, 11, NA, NA),
#     c( 7, 10, NA,  6, NA, NA)
#   )
# )


temperature_plots <- grid.arrange(
  grobs = list_ale_plots[c(1, 2, 8, 15, 13, 12, 7, 10)],
  layout_matrix = rbind(
    c(1,  4),
    c(2,  3),
    c(5, 6),
    c(7, 8)
  ),
  top = textGrob(
    "TEMPERATURE RELATED PREDICTORS\n", 
    gp = gpar(fontsize = 20, font = 3)
  )
)

photoperiode_plots <- grid.arrange(
  grobs = list_ale_plots[3],
  layout_matrix = rbind(1),
  top = textGrob(
    "PHOTOPERIOD\n", 
    gp = gpar(fontsize = 20, font = 3)
  )
)

radiation_plots <- grid.arrange(
  grobs = list_ale_plots[c(5,9,11,6)],
  layout_matrix = rbind(
    c(5),
    c(6),
    c(9),
    c(11)
  ),
  top = textGrob(
    "GLOBAL RADIATION\n", 
    gp = gpar(fontsize = 20, font = 3)
  )
)

wind_plots <- grid.arrange(
  grobs = list_ale_plots[4],
  layout_matrix = rbind(1),
  top = textGrob(
    "WIND\n", 
    gp = gpar(fontsize = 20, font = 3)
  )
)

rain_plots <- grid.arrange(
  grobs = list_ale_plots[14],
  layout_matrix = rbind(1),
  top = textGrob(
    "RAIN - EVAPOTRANSP.\n", 
    gp = gpar(fontsize = 20, font = 3)
  )
)

fig4 <- grid.arrange(
  grobs = list(
    temperature_plots,
    photoperiode_plots,
    radiation_plots,
    wind_plots,
    rain_plots
  ),
  layout_matrix = rbind(
    c(1, 1, 2, 3, 4, 5),
    c(1, 1, NA, 3, NA, NA),
    c(1, 1, NA, 3, NA, NA),
    c(1, 1, NA, 3, NA, NA)
  )
)

ggsave(
  filename = "D:/Mes Donnees/Terres_inovia/Article/Figures_pdf/Figure_4.pdf", 
  plot = fig4,
  height = 6*3,
  width = 11*3,
  units = "in"
)

# Figure 5 (AUC plot) -----------------------------------------------------

row_data_min <- f_vc_ROC(data_ranger_minimal)

data_min <- row_data_min[[1]]

roc_min <- roc(
  response = data_min$data, 
  predictor = data_min$proba,
  levels = c("Abs", "Pres")
)

row_data_full <- f_vc_ROC(data_ranger)

data_full <- row_data_full[[1]]

roc_full <- roc(
  response = data_full$data, 
  predictor = data_full$proba,
  levels = c("Abs", "Pres")
)

best_threshold_min <- coords(
  roc = roc_min, 
  x = "best", 
  input = "threshold",
  best.method = "closest.topleft"
) 

{
  pdf(
    file = "D:/Mes Donnees/Terres_inovia/Article/Figures_pdf/Figure_5.pdf",
    height = 8,
    width = 8,
  )
  
  plot(
    roc_min,
    xlim = c(1, 0),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i",
    lwd = 3.2,
    col = "#cd3700",
    font.lab = 2,
    
    # permet de ne pas imposer de ratio : origine a c(1, 0) impossible sans ca
    asp = NA,
    
    # threshold (value, sensitivity, specificity)
    print.thres = FALSE,
    print.thres.pch = 3,
    print.thres.col = "#cd3700",
    print.thres.pattern = "%.2f (%.2f, %.2f)",
    print.thres.cex = 2,
    
    # AUC
    print.auc = TRUE,
    print.auc.pattern = "AUC_min = %.3f",
    print.auc.cex = 2,
    print.auc.x = 0.65,
    print.auc.y = 0.5,
    auc.polygon = TRUE
  )
  text(
    x = best_threshold_min$specificity,
    y = best_threshold_min$sensitivity,
    labels = "+",
    cex = 3,
    font = 2,
    col = "#cd3700"
  )
  text(
    x = best_threshold_min$specificity - 0.20,
    y = best_threshold_min$specificity + 0.00,
    labels = paste0(
      round(best_threshold_min$threshold, 2),
      " (",
      round(best_threshold_min$sensitivity, 2),
      ", ", 
      round(best_threshold_min$specificity, 2),
      ")"
    ),
    cex = 2,
    font = 1,
    col = "#cd3700"
  )
  
  plot(
    roc_full,
    add = TRUE, 
    lty = "dashed",
    xlim = c(1, 0),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i",
    lwd = 3.2,
    col = "#638819",
    
    # permet de ne pas imposer de ratio : origine a c(1, 0) impossible sans ca
    asp = NA,
    
    # threshold (value, sensitivity, specificity)
    print.thres = FALSE,
    print.thres.pch = 3,
    print.thres.col = "#638819",
    print.thres.pattern = "%.2f (%.2f, %.2f)",
    print.thres.cex = 2,
    
    # AUC
    print.auc = TRUE,
    print.auc.pattern = "AUC_full = %.3f",
    print.auc.cex = 2,
    print.auc.x = 0.65,
    print.auc.y = 0.4,
    auc.polygon = FALSE
  )
  dev.off()
}


{
  png(
    file = "D:/Mes Donnees/Terres_inovia/Article/Figures_png/Figure_5_both.png",
    height = 600,
    width = 600,
  )
  
  plot(
    roc_min,
    xlim = c(1, 0),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i",
    lwd = 3.2,
    col = "#cd3700",
    font.lab = 2,
    
    # permet de ne pas imposer de ratio : origine a c(1, 0) impossible sans ca
    asp = NA,
    
    # threshold (value, sensitivity, specificity)
    print.thres = FALSE,
    print.thres.pch = 3,
    print.thres.col = "#cd3700",
    print.thres.pattern = "%.2f (%.2f, %.2f)",
    print.thres.cex = 2,
    
    # AUC
    print.auc = TRUE,
    print.auc.pattern = "AUC_min = %.3f",
    print.auc.cex = 2,
    print.auc.x = 0.65,
    print.auc.y = 0.5,
    auc.polygon = TRUE
  )
  text(
    x = best_threshold_min$specificity,
    y = best_threshold_min$sensitivity,
    labels = "+",
    cex = 3,
    font = 2,
    col = "#cd3700"
  )
  text(
    x = best_threshold_min$specificity - 0.20,
    y = best_threshold_min$specificity + 0.00,
    labels = paste0(
      round(best_threshold_min$threshold, 2),
      " (",
      round(best_threshold_min$sensitivity, 2),
      ", ",
      round(best_threshold_min$specificity, 2),
      ")"
    ),
    cex = 2,
    font = 1,
    col = "#cd3700"
  )
  
  plot(
    roc_full,
    add = TRUE,
    lty = "dashed",
    xlim = c(1, 0),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i",
    lwd = 3.2,
    col = "#638819",
    
    # permet de ne pas imposer de ratio : origine a c(1, 0) impossible sans ca
    asp = NA,
    
    # threshold (value, sensitivity, specificity)
    print.thres = FALSE,
    print.thres.pch = 3,
    print.thres.col = "#638819",
    print.thres.pattern = "%.2f (%.2f, %.2f)",
    print.thres.cex = 2,
    
    # AUC
    print.auc = TRUE,
    print.auc.pattern = "AUC_full = %.3f",
    print.auc.cex = 2,
    print.auc.x = 0.65,
    print.auc.y = 0.4,
    auc.polygon = FALSE
  )
  dev.off()
}


# Figure 6 (carte AUC) ----------------------------------------------------

predata_fig6 <- bsv %>% 
  select(
    Id_plot, 
    Latitude2, 
    Longitude2,
    all_of(names(data_ranger_minimal))
  ) %>% 
  group_by(Id_plot) %>% 
  slice_head() %>% 
  ungroup() %>% 
  st_as_sf(coords = c("Longitude2", "Latitude2"), crs = 4326)

depts <- st_within(predata_fig6, fondCarte)
depts <- map_int(1:5932, ~chuck(depts, .x))
depts <- ifelse(depts > 19, depts + 1, depts)

predata_fig6 <- predata_fig6 %>% 
  mutate(CODE_DEPT = depts) %>% 
  mutate(
    CODE_DEPT = sprintf("%02d", CODE_DEPT),
    CODE_DEPT = as.character(CODE_DEPT)
  ) %>% 
  left_join(fondCarte %>% as_tibble() %>% select(CODE_DEPT, NOM_DEPT), by = "CODE_DEPT") %>% 
  as.data.frame() %>% 
  select(Id_plot, CODE_DEPT, NOM_DEPT)

data_fig6 <- bsv %>% 
  select(
    Id_plot, 
    Latitude2, 
    Longitude2,
    all_of(names(data_ranger_minimal))
  ) %>% 
  left_join(predata_fig6, by = "Id_plot")

data_pred <- f_vc_ROC(data_fig6 %>% select(all_of(names(data_ranger_minimal))))

data_fig6_test <- data_fig6 %>% 
  arrange(Campagne) %>%
  bind_cols(proba = data_pred[[1]]$proba)

n_obs <- data_fig6_test %>% 
  group_by(CODE_DEPT) %>% 
  summarise(N_idplots = length(unique(Id_plot))) %>% 
  filter(N_idplots >= 10)

f_roc_dep <- function(code_dep, donnees){
  data <- donnees %>% 
    filter(CODE_DEPT %in% code_dep)
  if(code_dep %in% n_obs$CODE_DEPT){
    roc <- roc(
      response = data$Capture, 
      predictor = data$proba,
      levels = c("Abs", "Pres")
    )
  }
  else{
    roc <- NA
  }
  return(roc$auc)
}

auc_depts <- tibble(
  CODE_DEPT = unique(data_fig6_test$CODE_DEPT),
  AUC = map_dbl(
    .x = unique(data_fig6_test$CODE_DEPT), 
    .f = possibly(f_roc_dep, NA), 
    donnees = data_fig6_test
  )
)


data_fig6_test <- data_fig6_test %>% 
  select(
    Latitude2,
    Longitude2,
    Campagne, 
    CODE_DEPT,
    NOM_DEPT
  ) %>% 
  left_join(auc_depts, by = "CODE_DEPT") %>% 
  group_by(CODE_DEPT) %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(NOM_DEPT, AUC)

data_fig6_test <- fondCarte %>% 
  left_join(data_fig6_test) 

data_text <- data_fig6_test %>% 
  mutate(
    centroid_lat = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(Y),
    centroid_long = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(X)
  ) %>% 
  as.data.frame() %>% 
  select(AUC, centroid_lat, centroid_long) %>% 
  mutate(AUC = round(AUC, 2))

fig6 <- ggplot()+
  geom_sf(data = data_fig6_test, aes(fill = AUC))+
  scale_fill_gradientn(
    colors = c("#c77558", "#dfcc48", "#55977a"),
    # breaks = seq(0.5, 1, 0.1),
    # limits = c(0.5, 1)
  )+
  labs(
    x = "",
    y = "",
    fill = "Mean AUC"
  )+
  geom_text(
    data = data_text, 
    inherit.aes = FALSE, 
    aes(
      x = centroid_long, 
      y = centroid_lat,
      label = AUC,
      fontface = 2
    )
  )+
  theme(
    legend.title = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

fig6

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Review/Figures_pdf/Figure_7.pdf", 
  plot = fig6,
  height = 9,
  width = 9,
  units = "in"
)

# Corplot des 100 variables RFE -------------------------------------------

data_figx <- bsv %>% 
  select(all_of(rfe$optVariables))

ggplot()+
  geom_sf(data = data_fig6_test, aes(fill = AUC))+
  scale_fill_gradientn(
    colors = c("#c77558", "#dfcc48", "#55977a"),
    # breaks = seq(0.5, 1, 0.1),
    # limits = c(0.5, 1)
  )+
  labs(
    x = "",
    y = "",
    fill = "Mean AUC"
  )+
  geom_text(
    data = data_text, 
    inherit.aes = FALSE, 
    aes(
      x = centroid_long, 
      y = centroid_lat,
      label = AUC,
      fontface = 2
    )
  )+
  theme(
    legend.title = element_text(
      face = "bold"
    )
  )

matrix <- cor(data_figx)
corrplot(
  matrix,
  type = "upper",
  tl.cex = 0.5
)
summary(matrix)

flatten <- f_flattenCorrMatrix(matrix, 0.75)

matrix_min <- cor(data_ranger_minimal %>% select(-Campagne, -Capture))
flatten_min <- f_flattenCorrMatrix(matrix_min, 0.75)
corrplot(
  matrix_min,
  type = "upper",
  tl.cex = 1
)

# Figure sup 1 ------------------------------------------------------------

data_fig6_bis <- data_fig6_test %>% 
  left_join(n_obs, by = "CODE_DEPT")

plot(AUC ~ N_idplots, data = data_fig6_bis)

AUC_nidplots <- ggplot(data_fig6_bis, aes(x = N_idplots, y = AUC))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, colour = "blue")+
  labs(
    x = "Number of unique fields per French department",
    y = "Mean AUC\n"
  )+
  scale_y_continuous(breaks = seq(0.55, 0.85, 0.1))+
  theme(
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = 14)
  )

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Review/Figures_pdf/figsup_SI2.pdf", 
  plot = AUC_nidplots,
  height = 9,
  width = 9,
  units = "in"
)

data_fig6_bis2 <- bsv %>% 
  left_join(predata_fig6, by = "Id_plot") %>% 
  left_join(data_fig6_test %>% select(CODE_DEPT, AUC), by = "CODE_DEPT") %>% 
  group_by(CODE_DEPT) %>% 
  count(CODE_DEPT)

d <- data_fig6 %>% 
  mutate(Capture01 = ifelse(Capture == "Abs", 0, 1)) %>% 
  group_by(CODE_DEPT) %>% 
  mutate(ratio_pres = sum(Capture01 == 1) / n()) %>% 
  slice_head() %>% 
  select(CODE_DEPT, ratio_pres)

data_fig6_bis <- data_fig6_bis %>% 
  left_join(d, by = "CODE_DEPT")

hist <- ggplot(data_fig6_bis, aes(x = ratio_pres))+
  geom_histogram(
    aes(y = after_stat(count / sum(count))), 
    color = "black", 
    fill = "grey65"
  )+
  scale_x_continuous(breaks = seq(0, 0.7, 0.1))+
  labs(
    x = "",
    y = "Frequency\n"
  )+
  scale_x_continuous(expand = c(0.05, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    plot.margin = margin(90, 30, 40, 30),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = 14), 
    panel.background = element_blank(),
    axis.line = element_line()
  )

hist

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Figures_pdf/figsup_hist.pdf", 
  plot = hist,
  height = 9,
  width = 9,
  units = "in"
)

data_text_freq <- data_fig6_bis %>% 
  mutate(
    centroid_lat = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(Y),
    centroid_long = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(X)
  ) %>% 
  as.data.frame() %>% 
  select(ratio_pres, centroid_lat, centroid_long) %>% 
  mutate(ratio_pres = round(ratio_pres, 2))

carte_f <- ggplot()+
  geom_sf(data = data_fig6_bis, aes(fill = ratio_pres))+
  scale_fill_gradient(low = "forestgreen", high = "firebrick")+
  labs(
    x = "",
    y = "",
    fill = "Occurence frequency"
  )+
  geom_text(
    data = data_text_freq, 
    inherit.aes = FALSE, 
    aes(
      x = centroid_long, 
      y = centroid_lat,
      label = ratio_pres,
      fontface = 2
    ),
    color = "grey"
  )+
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

carte_f

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Figures_pdf/figsup_carte.pdf", 
  plot = carte_f,
  height = 9,
  width = 9,
  units = "in"
)

both <- grid.arrange(carte_f, hist, nrow = 1)

both

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Review/Figures_pdf/Figure_2.pdf", 
  plot = both,
  height = 9,
  width = 18,
  units = "in"
)

m <- lm(AUC ~ poly(ratio_pres, 2) + N_idplots, data = data_fig6_bis %>% ungroup() %>% drop_na())

summary(m)

sink(file = "lm.txt")
print(summary(m))
sink()


dplot <- data_fig6_bis %>% 
  ungroup() %>% 
  drop_na()

plot_lm <- ggplot(dplot, aes(ratio_pres, AUC))+
  geom_point()+
  geom_smooth(data = dplot, method = "lm", formula = y ~ poly(x, 2), colour = "blue")+
  labs(
    x = expression(paste("Frequency of ", italic("C. napi "), "occurence among all observations per French department")),
    # x = "\nFrequency of C. napi occurence\namong all observations per French department",
    y = "Mean AUC\n"
  )+
  scale_y_continuous(breaks = seq(0.55, 0.85, 0.1))+
  theme(
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = 14)
  )

plot_lm

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Figures_pdf/figsup_lm.pdf", 
  plot = plot_lm,
  height = 9,
  width = 9,
  units = "in"
)

data_text_n <- data_fig6_bis %>% 
  mutate(
    centroid_lat = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(Y),
    centroid_long = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(X)
  ) %>% 
  as.data.frame() %>% 
  select(N_idplots, centroid_lat, centroid_long)

carte_n <- ggplot()+
  geom_sf(data = data_fig6_bis, aes(fill = N_idplots))+
  scale_fill_gradient()+
  labs(
    x = "",
    y = "",
    fill = "N fields"
  )+
  geom_text(
    data = data_text_n, 
    inherit.aes = FALSE, 
    aes(
      x = centroid_long, 
      y = centroid_lat,
      label = N_idplots,
      fontface = 2
    ),
    color = "grey"
  )+
  theme(
    legend.title = element_text(
      face = "bold"
    )
  )

{
  pdf(
    file = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Figures_pdf/carte_n.pdf",
    height = 6,
    width = 9,
  )
  print(carte_n)
  dev.off()
  }

# Figure sup 2 ------------------------------------------------------------

data_confmatmin <- data_min %>% 
  mutate(pred = ifelse(proba >= best_threshold_min$threshold, as.character("Pres"), as.character("Abs"))) %>% 
  mutate(pred = factor(pred, levels = c("Pres", "Abs")))

confmatmin <- confusionMatrix(data = data_confmatmin$pred, reference = data_confmatmin$data)

sink("confmatmin.txt")
print(confmatmin)
sink()

data_confmatdep <- data_fig6 %>% 
  bind_cols(pred = data_confmatmin$pred)

ls_dep <- map(
  .x = unique(data_confmatdep$NOM_DEPT),
  .f = ~data_confmatdep %>% filter(NOM_DEPT == .x)
) 

ls_confmat <- map(
  ls_dep,
  ~confusionMatrix(.x$pred, .x$Capture)
) %>% 
  setNames(unique(data_confmatdep$NOM_DEPT))

perf_bydep <- map_dfr(
  ls_confmat,
  ~.x$byClass[c("Pos Pred Value", "Neg Pred Value")] %>% 
    as.list() %>% 
    as_tibble()
) %>% 
  mutate(NOM_DEPT = names(ls_confmat)) %>% 
  left_join(data_fig6_bis %>% select(NOM_DEPT, ratio_pres), by = "NOM_DEPT")

fpr <- map_dbl(
  ls_confmat,
  ~.x$table[1, 2]/(.x$table[1, 2] + .x$table[2, 2])
)

fnr <- map_dbl(
  ls_confmat,
  ~.x$table[2, 1]/(.x$table[2, 1] + .x$table[1, 1])
)

perf_bydep <- perf_bydep %>% 
  mutate(
    false_negative = fnr,
    false_positive = fpr
  )

plot(`Pos Pred Value` ~ ratio_pres, data = perf_bydep)
points(`Neg Pred Value` ~ ratio_pres, data = perf_bydep, col = "red")

# Map DOY first flight ----------------------------------------------------

data_map_doy <- data_fig6 %>% 
  bind_cols(Date_obs = bsv$Date_obs, jour = bsv$jour) %>% 
  group_by(Id_plot) %>% 
  arrange(Date_obs) %>% 
  filter(Capture == "Pres") %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(CODE_DEPT, Campagne) %>% 
  mutate(median_j = median(jour)) %>% 
  slice_head() %>% 
  ungroup() %>% 
  left_join(fondCarte, by = "CODE_DEPT") %>% 
  st_as_sf()

data_text_ff <- data_map_doy %>% 
  mutate(
    centroid_lat = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(Y),
    centroid_long = st_centroid(.) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      pull(X)
  ) %>% 
  as.data.frame()

carte_ff <- ggplot()+
  geom_sf(data = fondCarte)+
  geom_sf(data = data_map_doy, aes(fill = median_j))+
  scale_fill_gradient(
    low = "palegreen",
    high = "black",
  )+
  labs(
    x = "",
    y = "",
    fill = "Median DOY of 1st flight"
  )+
  geom_text(
    data = data_text_ff, 
    inherit.aes = FALSE, 
    aes(
      x = centroid_long, 
      y = centroid_lat,
      label = round(median_j, 0),
      fontface = 2,
      size = 10
    ),
    color = "grey"
  )+
  scale_size(guide = 'none')+
  theme(
    legend.title = element_text(face = "bold"),
    strip.text = element_text(size = 14)
  )+
  facet_wrap(~Campagne)

carte_ff

ggsave(
  filename = "/home/legros/OneDrive/Articles/Modele_vol/Figures_pdf/figsup_DOY_map.pdf", 
  plot = carte_ff,
  height = 9*1.9,
  width = 16*1.9,
  units = "in"
)

facet_doy_lat <- ggplot(data_map_doy, aes(x = Latitude2, y = median_j))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, colour = "blue")+
  labs(
    x = "latitude (°)",
    y = "DOY of 1st capture",
  )+
  scale_x_continuous(breaks = seq(42.5, 50, 1.25))+
  scale_y_continuous(breaks = seq(15, 105, 15))+
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = 14)
  )+
  facet_wrap(~Campagne)

facet_doy_lat

ggsave(
  filename = "/home/legros/OneDrive/Articles/Modele_vol/Figures_pdf/figsup_DOY_lines.pdf", 
  plot = facet_doy_lat,
  height = 9,
  width = 16,
  units = "in"
)

facet_doy_lng <- ggplot(data_map_doy, aes(x = Longitude2, y = median_j))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, colour = "blue")+
  labs(
    x = "longitude (°)",
    y = "",
  )+
  scale_x_continuous(breaks = seq(-4, 8, 1))+
  scale_y_continuous(breaks = seq(15, 105, 15))+
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = 14)
  )+
  facet_wrap(~Campagne)

facet_doy_lng

both_facets <- grid.arrange(facet_doy_lat, facet_doy_lng, nrow = 1)

print(both_facets)

ggsave(
  filename = "/media/legros/Donnees/OneDrive_linux/Articles/Modele_vol/Review/Figures_pdf/figsup_SI1_3.pdf", 
  plot = both_facets,
  height = 9,
  width = 16,
  units = "in"
)

lm_doy <- lm(median_j ~ Latitude2 + Campagne, data = data_map_doy)
summary(lm_doy)
plot(lm_doy)
