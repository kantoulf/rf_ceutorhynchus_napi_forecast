# Packages ----------------------------------------------------------------
{
  library(sf)
  library(magrittr)
  library(lubridate)
  library(geosphere)
  library(furrr)
  library(raster)
  library(fasterize)
  library(Hmisc)
  library(tidyverse)
}


# Donnees generales -------------------------------------------------------
{
  bsv <- readRDS("data/export/rds/bsv_mai2022.rds")
  
  veg_raster <- raster::raster("data/import/bd_topo/veg_raster.tif")
  
  files <- list.files(path = "data/import/rpg/rasters/")
  files <- files[grepl("_raster", files)]
  files_paths <- list.files(path = "data/import/rpg/rasters/", full.names = T)
  files_paths <- files_paths[grepl("_raster", files_paths)]
  rasters <- map(files_paths, raster)
  names(rasters) <- 2010:2020
}


# options -----------------------------------------------------------------
{
  options(future.globals.maxSize= 891289600)
  memory.limit(50000)
}


# Fonctions ---------------------------------------------------------------
{
  
  f_foret <- function(rast = veg_raster, latitude, longitude, rayon, tokeep = c(1, 3:7, 9)){
    f_filter <- function(vec){
      0.04 * vec[vec %in% tokeep] %>% length()
    }
    raster::extract(
      x = rast, 
      y = matrix(c(longitude, latitude), ncol = 2), 
      fun = f_filter,
      buffer = rayon
    )
  }
  
  
  f_colza <- function(annee, latitude, longitude, rayon, lag = 0){
    annee <- annee - lag
    if(isTRUE(TRUE %in% grepl(paste0("rpg", annee), files))){
      r <- rasters[[as.character(annee)]]
      raster::extract(
        x = r, 
        y = matrix(c(longitude, latitude), ncol = 2), 
        buffer = rayon,
        fun = "sum"
      )
    }
    else{
      NA
    }
  }
}


# Ajout forêt -------------------------------------------------------------
rayons <- c(100, 500, 1000, 3000, 8000, 15000)

unique <- bsv %>% 
  group_by(Id_plot) %>% 
  slice_head() %>% 
  ungroup()

template <- tibble(
  Id_plot = rep(unique$Id_plot, length(rayons)),
  latitude = rep(unique$Latitude2, length(rayons)),
  longitude = rep(unique$Longitude2, length(rayons)),
  rayon = rep(rayons, each = nrow(unique))
)

plan(multisession, workers = 7)
system.time(res <- future_pmap_dbl(
  .l = template %>% dplyr::select(-Id_plot), 
  .f = f_foret,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE, scheduling = 50))
)
plan(sequential)

test <- template %>% 
  mutate(surf_foret = res) %>% 
  pivot_wider(
    values_from = surf_foret, 
    names_from = rayon
  ) %>% 
  rename(
    sforet_100 = "100",
    sforet_500 = "500",
    sforet_1000 = "1000",
    sforet_3000 = "3000",
    sforet_8000 = "8000",
    sforet_1500 = "15000"
  )

bsv <- bsv %>% 
  left_join(test %>% dplyr::select(Id_plot, contains("sforet")))


# Ajout colza -------------------------------------------------------------
unique <- bsv %>% 
  group_by(Id_plot) %>% 
  slice_head() %>% 
  ungroup()

t1 <- tibble(
  Id_plot = rep(unique$Id_plot, length(rayons)),
  annee = rep(year(unique$Date_obs), length(rayons)),
  latitude = rep(unique$Latitude2, length(rayons)),
  longitude = rep(unique$Longitude2, length(rayons)),
  rayon = rep(rayons, each = nrow(unique))
)

template <- rbind(t1, t1) %>% 
  mutate(lag = rep(c(0, 1), each = nrow(t1)))

plan(multisession, workers = 7)
res <- future_pmap_dbl(
  .l = template %>% dplyr::select(-Id_plot), 
  .f = f_colza,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE, scheduling = 50)
)
plan(sequential)

test <- template %>% 
  mutate(
    surf_colza_n = res,
    noms = paste(rayon, lag, sep = "_")
  ) %>% 
  dplyr::select(-rayon, -lag) %>% 
  pivot_wider(
    values_from = surf_colza_n, 
    names_from = noms
  ) %>% 
  arrange(annee, Id_plot) %>% 
  rename(
    scolza_now_100 = "100_0",
    scolza_now_500 = "500_0",
    scolza_now_1000 = "1000_0",
    scolza_now_3000 = "3000_0",
    scolza_now_8000 = "8000_0",
    scolza_now_15000 = "15000_0",
    scolza_lag_100 = "100_1",
    scolza_lag_500 = "500_1",
    scolza_lag_1000 = "1000_1",
    scolza_lag_3000 = "3000_1",
    scolza_lag_8000 = "8000_1",
    scolza_lag_15000 = "15000_1"
  )

bsv <- bsv %>% 
  left_join(test %>% dplyr::select(Id_plot, contains("scolza")))

bsv <- bsv %>% 
  mutate(
    Capture = ifelse(Valeur > 0, "Pres", "Abs"),
    Capture = factor(Capture, levels = c("Pres", "Abs"))
  ) %>% 
  relocate(Id_plot, Semaine, Reseau, Date_obs, Stationproche, Valeur, Campagne, Capture) %>%
  mutate(
    scolza_diff_100 = scolza_now_100 - scolza_lag_100,
    scolza_diff_500 = scolza_now_500 - scolza_lag_500,
    scolza_diff_1000 = scolza_now_1000 - scolza_lag_1000,
    scolza_diff_3000 = scolza_now_3000 - scolza_lag_3000,
    scolza_diff_8000 = scolza_now_8000 - scolza_lag_8000,
    scolza_diff_15000 = scolza_now_15000 - scolza_lag_15000
  )

saveRDS(bsv, "data/export/rds/bsv_mai2022.rds")
