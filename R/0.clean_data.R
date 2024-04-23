# Packages ----------------------------------------------------------------
{
  library(tidyverse)
  library(lubridate)
}

# Data --------------------------------------------------------------------
data <- read.csv(file = "data/import/bsv/bsv_complet_2011_2022.csv") %>% 
  mutate(Date_obs = ymd(Date_obs))


# Main code ---------------------------------------------------------------

# Recodage des id_plots utilises plus d'une fois.
# exemple id_plot 2347 utilise pour un champ en 2009 et 2010.
# on veut un idplot pour un champ-annee uniquement.

# On cherche les idplot pour lesquels il y a plusieurs annees.
# Dans certains cas, c'est normal car le suivi commence en decembre et continue au printemps.
# Critere pour savoir s'il faut recoder : s'il y a plus de 2 annees differentes pour un id_plot

double <- data %>%
  group_by(Id_plot) %>%
  summarise(
    min = min(Date_obs),
    max = max(Date_obs),
    ymin = year(min),
    ymax = year(max),
    diff = ymax - ymin
  ) %>%
  mutate(double = case_when(ymin == ymax ~ FALSE, TRUE ~ TRUE)) %>%
  filter(double == TRUE)

toRecode <- double %>%
  filter(diff > 1) %>%
  pull(Id_plot)

# On ne recode que si c'est necessaire
if(length(toRecode > 0)){
  
  test <- bsv %>%
    mutate(id_unique = paste0(id_plot, latitude, longitude))
  
  df_idunique <- tibble(
    id_unique = unique(test$id_unique),
    id = 1:length(unique(test$id_unique))
  )
  
  data <- test %>%
    left_join(df_idunique) %>%
    dplyr::select(-id_plot, -id_unique) %>%
    rename(id_plot = id) %>%
    relocate(id_plot)
  
  double <- data %>%
    group_by(id_plot) %>%
    summarise(
      min = min(date_obs),
      max = max(date_obs)
    )
  
  data <- data %>%
    left_join(double %>% select(id_plot, min, max)) %>%
    rename(date_min = min, date_max = max)
}

# Suppression Id_plot obervés 1 fois --------------------------------------

# On estime que les parcelles ou un seul releve a ete effectuee ne sont pas dignes de confiance.
# On les retire du jeu de donnees.

to_remove <- data %>% 
  group_by(Id_plot) %>% 
  summarise(N = n()) %>% 
  filter(N == 1) %>% 
  pull(Id_plot)

data <- data %>% 
  filter(!Id_plot %in% to_remove)


# Imputation des Valeurs = 0 ----------------------------------------------

# On veut un tableau avec des captures chaque semaine.
# Si deux releves consecutifs sont separes par plus de 1 semaine, et qu'il n'y a pas de captures dans le second,
# On peut inferer qu'il n y a pas eu de captures entre.
# Exemple
# semaine 7 : 3 charançons
# semaine 8 : NA
# semaine 9 : 0 charançons (= somme des captures sur semaine 8 et 9)
# => on est surs qu'il n y a pas eu de captures en semaine 8, donc on impute comme suit :
# semaine 7 : 3 charançons
# semaine 8 : 0 charançons
# semaine 9 : 0 charançons

data_imp <- tibble()

# Code de Celia
for (id in unique(data$Id_plot)){
  
  t <- seq(
    data %>% 
      filter(Id_plot == id) %>% 
      pull(Semaine) %>% 
      min(), 
    data %>% 
      filter(Id_plot == id) %>% 
      pull(Semaine) %>% 
      max()
  ) %>% 
    as_tibble() %>% 
    rename(Semaine = value) %>% 
    full_join(
      data %>% 
        filter(Id_plot == id) %>% 
        select(Id_plot, Reseau, Longitude2, Latitude2, Campagne, Date_obs, Semaine, Valeur) %>% 
        arrange(Semaine), 
      by = "Semaine"
    ) %>% 
    as.data.frame()
  
  for (ind in (sort(which(is.na(t$Valeur)), decreasing = TRUE))){
    if (!is.na(t[ind+1, "Valeur"]) & t[ind+1, "Valeur"] == 0){
      t[ind, "Valeur"] <- 0
    }
    d1 <- as.Date(t[ind + 1, "Date_obs"])
    i <- 1
    while(is.na(as.Date(t[ind - i, "Date_obs"]))){
      i = i + 1
    }
    d2 <- as.Date(t[ind-i, "Date_obs"])
    t[ind, "Date_obs"] <- seq.Date(d1 , d2, length.out = i+2)[2]
    t[ind, c("Id_plot", "Reseau", "Longitude2", "Latitude2", "Campagne")] <- unique(t[!is.na(t$Id_plot), c("Id_plot", "Reseau", "Longitude2", "Latitude2", "Campagne")])
  }
  
  data_imp <- data_imp %>% 
    bind_rows(t)
}

# Suppression si Valeur positive suivant semaine manquante ----------------

# Exemple :
#
# semaine 7 : 3 charançons
# semaine 8 : NA
# semaine 9 : 15 charançons (= somme des captures sur semaine 8 et 9)
#
# => on ne sait pas si les charançons ont etes captures en semaine 8 ou 9
# => trop imprecis, donc on supprime ces parcelles
# Un peu brutal, on aurait pu virer les observations problematiques plutot que les parcelles entieres

parc_na <- data_imp %>% 
  filter(is.na(Valeur)) %>% 
  distinct(Id_plot) %>% 
  pull()

data_imp <- data_imp %>% 
  filter(!Id_plot %in% parc_na)


# Export data -------------------------------------------------------------
saveRDS(data_imp, "data/export/rds/bsv_brut_clean.rds")

