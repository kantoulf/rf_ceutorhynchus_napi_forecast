# Packages ----------------------------------------------------------------
{
  library(sf)
  library(magrittr)
  library(osmdata)
  library(lubridate)
  library(MeteoAPI)
  library(geosphere)
  library(furrr)
  library(arvalis.climbox)
  library(RANN)
  library(janitor)
  library(data.table)
  library(tidyverse)
}


# Data --------------------------------------------------------------------
{
  bsv_brut <- readRDS("data/export/rds/bsv_brut_clean.rds")
  
  bsv_idplot_unique <- bsv_brut %>%
    group_by(Id_plot) %>%
    slice_head() %>%
    ungroup()
  
  stations <- MeteoAPI::info_stations(stations = "")
  
  load("./data/import/safran/fichiers_rda/quantiles_vent.rda")
}


# options -----------------------------------------------------------------
options(future.globals.maxSize = 4258291200)


# Fonctions ---------------------------------------------------------------
{
  f_parallel <- function(x = parallel::detectCores() - 1){
    plan(multisession, workers = x)
  }
  
  f_class <- function(dataframe){
    map_dfr(dataframe, class)
  }
  
  f_choix_stations <- function(idplot){
    ligne <- bsv_idplot_unique %>% filter(Id_plot == idplot) 
    campagne <- ligne %>% pull(Campagne)
    date_debut <- ymd(paste0(campagne - 1, "-06-30"))
    date_fin <- ligne$Date_obs
    stations_filtre <- stations %>%
      filter(ymd(DonneesDateDebut) <= date_debut, ymd(DonneesDateFin) >= date_fin) 
    dist <- distm(as.matrix(ligne[,c("Longitude2", "Latitude2")]), as.matrix(stations_filtre[,c("Longitude", "Latitude")]), fun = distGeo)
    stations_filtre %>% slice(which(dist == min(dist))) %>% pull(CodeStation)
  }
  
  mdMoyenne_vec <- Vectorize(mdMoyenne, vectorize.args = c("Station", "DateDebut", "DateFin"))
  mdSomme_vec <- Vectorize(mdSomme, vectorize.args = c("Station", "DateDebut", "DateFin"))
  mdSommeSup_vec <- Vectorize(mdSommeSup, vectorize.args = c("Station", "DateDebut", "DateFin"))
  mdJours_vec <- Vectorize(mdJours, vectorize.args = c("Stations", "Jours", "JourFin"))
  mdExtreme_vec <- Vectorize(mdExtreme, vectorize.args = c("Station", "DateDebut", "DateFin"))
  mdOccurrence_vec <- Vectorize(mdOccurrence, vectorize.args = c("Station", "DateDebut", "DateFin"))
  
  f_dates_hiver <- Vectorize(function(date){
    year <- year(date)
    ymd(paste0(year, "-01-01"))
  })
  
  f_dates_ete <- Vectorize(function(date){
    year <- year(date)
    ymd(paste0(year - 1, "-07-01"))
  })
  
  f_meteo <- function(lag, vec_station, vec_dateobs){
    
    datesEte <- as_date(f_dates_ete(vec_dateobs))
    datesHiver <- as_date(f_dates_hiver(vec_dateobs))
    
    data.frame(
      # SEMAINES LAG
      "ampTmin" = mdExtreme_vec("Amplitude", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, TypeExtreme = "Minimum", "%Y-%m-%d"),
      "ampTmoy" = mdMoyenne_vec("Amplitude", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "ampTmax" = mdExtreme_vec("Amplitude", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, TypeExtreme = "Maximum", "%Y-%m-%d"),
      "sTmoy" = mdSomme_vec("TMoy", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmoy_6" = mdSommeSup_vec("TMoy", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 6, 99, "%Y-%m-%d"),
      "sTmax" = mdSomme_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmax_1" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 1, 99, "%Y-%m-%d"),
      "sTmax_3" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 3, 99, "%Y-%m-%d"),
      "sTmax_5" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 5, 99, "%Y-%m-%d"),
      "sTmax_7" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 7, 99, "%Y-%m-%d"),
      "sTmax_9" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 9, 99, "%Y-%m-%d"),
      "sTmax_12" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 12, 99, "%Y-%m-%d"),
      "sTmax_15" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 15, 99, "%Y-%m-%d"),
      "sTmax_17" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 17, 99, "%Y-%m-%d"),
      "sTmax_19" = mdSommeSup_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 19, 99, "%Y-%m-%d"),
      "sPluie" = mdSomme_vec("Pluie", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "sInsolation" = mdSomme_vec("Insolation", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "tMax" = mdExtreme_vec("TMaxi", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "Maximum", "%Y-%m-%d"),
      "tMin" = mdExtreme_vec("TMini", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "Minimum", "%Y-%m-%d"),
      "nJours_tMax_9_int"= mdOccurrence_vec("TMax", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 9, 99, "%Y-%m-%d"),
      "nJours_sol_3_int" = mdOccurrence_vec("Insolation", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, 3, 99, "%Y-%m-%d"),
      "ETPmoy" = mdMoyenne_vec("ETP", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "ETPmax" = mdExtreme_vec("ETP", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, TypeExtreme = "Maximum", "%Y-%m-%d"),
      "insolationMoy" = mdMoyenne_vec("Insolation", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "insolationMax" = mdExtreme_vec("Insolation", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, TypeExtreme = "Minimum", "%Y-%m-%d"),
      "RGmoy" = mdMoyenne_vec("RG joules", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "photop" = mdMoyenne_vec("Photopériode", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "pluieMoy" = mdMoyenne_vec("Pluie", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      "pluieMax" = mdExtreme_vec("Pluie", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, TypeExtreme = "Maximum", "%Y-%m-%d"),
      "PETPmoy" = mdMoyenne_vec("P-ETP", vec_station, vec_dateobs - days(6) - lag, vec_dateobs - lag, "%Y-%m-%d"),
      # HIVER
      "sPluie_hiver" = mdSomme_vec("Pluie", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmax_hiver" = mdSomme_vec("TMaxi", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sSol_hiver" = mdSomme_vec("Insolation", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sRGmoy_hiver" = mdSomme_vec("RG joules", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sETP_hiver" = mdSomme_vec("ETP", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sPETP_hiver" = mdSomme_vec("P-ETP", vec_station, datesHiver, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmax_7_hiver" = mdSommeSup_vec("TMax", vec_station, datesHiver, vec_dateobs - lag, 7, 99, "%Y-%m-%d"),
      "sTmax_9_hiver" = mdSommeSup_vec("TMax", vec_station, datesHiver, vec_dateobs - lag, 9, 99, "%Y-%m-%d"),
      "sTmax_12_hiver" = mdSommeSup_vec("TMax", vec_station, datesHiver, vec_dateobs - lag, 12, 99, "%Y-%m-%d"),
      "sTmax_15_hiver" = mdSommeSup_vec("TMax", vec_station, datesHiver, vec_dateobs - lag, 15, 99, "%Y-%m-%d"),
      # ETE
      "sPluie_ete" = mdSomme_vec("Pluie", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmax_ete" = mdSomme_vec("TMaxi", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sSol_ete" = mdSomme_vec("Insolation", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sRGmoy_ete" = mdSomme_vec("RG joules", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sETP_ete" = mdSomme_vec("ETP", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sPETP_ete" = mdSomme_vec("P-ETP", vec_station, datesEte, vec_dateobs - lag, "%Y-%m-%d"),
      "sTmax_7_ete" = mdSommeSup_vec("TMax", vec_station, datesEte, vec_dateobs - lag, 7, 99, "%Y-%m-%d"),
      "sTmax_9_ete" = mdSommeSup_vec("TMax", vec_station, datesEte, vec_dateobs - lag, 9, 99, "%Y-%m-%d"),
      "sTmax_12_ete" = mdSommeSup_vec("TMax", vec_station, datesEte, vec_dateobs - lag, 12, 99, "%Y-%m-%d"),
      "sTmax_15_ete" = mdSommeSup_vec("TMax", vec_station, datesEte, vec_dateobs - lag, 15, 99, "%Y-%m-%d")
    ) %>% 
      set_colnames(paste0(names(.), "_", lag))
  }
  
  f_prepare <- function(annee, df = bsv){
    data_bsv <- df %>% 
      filter(Campagne == annee)
    grille <- data2save %>% 
      filter(DATE == ymd(paste0(annee, "-01-01"))) %>% 
      select(latitude, longitude)
    idplots <- data_bsv %>% 
      distinct(Id_plot, .keep_all = T)
    nearest <- nn2(
      data = grille, 
      query = idplots %>% select(Latitude2, Longitude2), 
      k = 1
    )
    grille_nearest <- grille[nearest$nn.idx,] %>% 
      rename(
        lat_nearest = latitude, 
        long_nearest = longitude
      )
    idplots <- idplots %>% 
      bind_cols(grille_nearest) %>% 
      select(Id_plot, lat_nearest, long_nearest)
    data_bsv %>% 
      left_join(idplots)
  }
  
  f_indicVent <- function(num_row){
    line <- data_bsv %>% 
      slice(num_row)
    dateObs <- line$Date_obs
    date0 <- line$Date_obs - days(6)
    data2save <- data2save %>% 
      filter(
        DATE >= date0 & DATE <= dateObs,
        latitude == line$lat_nearest,
        longitude == line$long_nearest
      )
    data2return <- tibble(
      min_vent = min(data2save$FF_Q),
      moy_vent = mean(data2save$FF_Q),
      max_vent = max(data2save$FF_Q),
      nj_qt_01 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[2]]),
      nj_qt_02 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[3]]),
      nj_qt_03 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[4]]),
      nj_qt_04 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[5]]),
      nj_qt_05 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[6]]),
      nj_qt_06 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[7]]),
      nj_qt_07 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[8]]),
      nj_qt_08 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[9]]),
      nj_qt_09 = length(data2save$FF_Q[data2save$FF_Q > quantiles_vent[10]])
    )
    return(data2return)
  }
  
  f_vent <- function(annee){
    load(paste0("data/import/safran/safran_dt_", annee, ".rda"), envir = .GlobalEnv)
    data_bsv <<- f_prepare(annee)
    plan(multisession, workers = parallel::detectCores() - 1)
    toBind <- future_map_dfr(
      .x = 1:nrow(data_bsv), 
      .f = f_indicVent, 
      .options = furrr_options(seed = TRUE)
    )
    plan(sequential)
    return(data_bsv %>% bind_cols(toBind) %>% select(-lat_nearest, -long_nearest))
  }
  
  f_climbox <- function(nrow){
    id <- df_climbox %>% slice(nrow) %>% pull(Id_plot)
    as.data.frame(
      mdJour(
        c("TMaxi", "Insolation", "Pluie"), 
        df_climbox$Stationproche[nrow], 
        seq.Date(df_climbox$Date_min[nrow], df_climbox$Date_max[nrow], by = "day"), 
        format = "%Y-%m-%d"
      )
    ) %>% 
      mutate(Id_plot = id)
  }
}


# Ajout stations proches --------------------------------------------------
f <- possibly(.f = f_choix_stations, otherwise = NA)

f_parallel()
stations_proches <- future_map_int(bsv_idplot_unique$Id_plot, f)

bsv_idplot_unique <- bsv_idplot_unique %>% 
  mutate(Stationproche = stations_proches)

bsv <- bsv_brut %>% 
  left_join(bsv_idplot_unique %>% select(Id_plot, Stationproche))


# Ajout meteo -------------------------------------------------------------
df_meteo <- future_map_dfc(
  .x = seq(0, 35, 7),
  vec_station = bsv$Stationproche,
  vec_dateobs = bsv$Date_obs,
  .f = f_meteo,
  .options = furrr_options(seed = T)
)
plan(sequential)

bsv <- bsv %>% 
  bind_cols(df_meteo) %>% 
  filter(ampTmax_0 > -9999)


# Ajout vent --------------------------------------------------------------
bsv <- bsv %>% 
  mutate(Capture = ifelse(Valeur > 0, "Presence", "Absence")) %>% 
  relocate(Id_plot : Reseau, Stationproche, Date_obs, Valeur, Campagne, Capture) %>% 
  mutate(
    Capture = ifelse(Capture == "Presence", "Pres", "Abs"),
    Capture = factor(Capture, levels = c("Pres", "Abs"))
  )

bsv <- map_dfr(unique(bsv$Campagne), f_vent)

saveRDS(bsv, "data/export/rds/bsv_mai2022.rds")

# Creation BSV etendu -----------------------------------------------------

bsv_etendu <- bsv %>% 
  select(c(1:10, contains("sforet"), contains("scolza"))) %>% 
  group_by(Id_plot) %>% 
  mutate(min_date = ymd(paste0(Campagne - 1, "-12-20"))) %>% 
  complete(Date_obs = seq.Date(min(min_date), max(Date_obs), by = "day")) %>% 
  fill(c(Id_plot, Reseau, Stationproche, Campagne, Latitude2, Longitude2, contains("sforet"), contains("scolza")), .direction = "up") %>% 
  ungroup() %>% 
  select(-min_date) %>% 
  arrange(Id_plot, Date_obs) %>% 
  mutate(jour = yday(Date_obs))

df_climbox <- bsv_etendu %>% 
  group_by(Id_plot) %>% 
  summarise(
    Stationproche = Stationproche,
    Date_min = min(Date_obs),
    Date_max = max(Date_obs)
  ) %>% 
  slice_head() %>% 
  ungroup()

f_parallel()
df_meteo <- future_map_dfr(
  .x = 1:nrow(df_climbox), 
  .f = f_climbox, 
  .options = furrr_options(seed = T)
)
plan(sequential)

n_meteo <- df_meteo %>% 
  group_by(Id_plot) %>% 
  summarise(N_test = n())

n_bsv <- bsv_etendu %>% 
  group_by(Id_plot) %>% 
  summarise(N_bsv = n())

n_bind <- n_meteo %>% 
  bind_cols(n_bsv %>% select(N_bsv)) %>% 
  mutate(equal = ifelse(N_test == N_bsv, TRUE, FALSE))

to_remove <- n_bind %>% filter(equal == FALSE) %>% pull(Id_plot)

df_meteo <- df_meteo %>% 
  filter(!Id_plot %in% to_remove)

bsv_etendu <- bsv_etendu %>% 
  filter(!Id_plot %in% to_remove) %>% 
  bind_cols(df_meteo %>% select(-Id_plot))

bsv_etendu <- bsv_etendu %>% 
  group_by(Id_plot) %>% 
  mutate(
    toRemove = ifelse(lead(jour) == jour, TRUE, FALSE),
    toRemove = ifelse(is.na(toRemove), FALSE, toRemove)
  ) %>% 
  ungroup()

bsv_etendu <- bsv_etendu %>% 
  filter(!toRemove) %>% 
  select(-toRemove)


# Ajout meteo BSV etendu --------------------------------------------------
f_parallel()
df_meteo_etendu <- future_map_dfc(
  .x = seq(0, 35, 7),
  vec_station = bsv_etendu$Stationproche,
  vec_dateobs = bsv_etendu$Date_obs,
  .f = f_meteo,
  .options = furrr_options(seed = T),
  .progress = TRUE
)
plan(sequential)

bsv_etendu <- bsv_etendu %>% 
  bind_cols(df_meteo_etendu) %>% 
  filter(ampTmax_0 > -9999)

bsv_etendu <- bsv_etendu %>% 
  mutate(
    TMaxi = mdJours_vec("Tmaxi", .$Stationproche, as.character(.$Date_obs), as.character(.$Date_obs), "%Y-%m-%d"),
    Pluie = mdJours_vec("Pluie", .$Stationproche, as.character(.$Date_obs), as.character(.$Date_obs), "%Y-%m-%d"),
    Insolation = mdJours_vec("Insolation", .$Stationproche, as.character(.$Date_obs), as.character(.$Date_obs), "%Y-%m-%d")
  )


# Ajout vent BSV etendu ---------------------------------------------------
bsv_etendu <- map_dfr(unique(bsv_etendu$Campagne), f_vent, df = bsv_etendu)

bsv_etendu <- bsv_etendu %>% 
  bind_cols(df_meteo_etendu %>% select(-Id_plot, -Date_obs)) %>% 
  select(-c(toRemove, id_safran))

saveRDS(bsv_etendu, "data/export/rds/bsv_etendu_mai2022.rds")
