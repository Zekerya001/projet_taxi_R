# =============================================================================
# PROJET R - Master SSD | Semestre 2 - 2025/2026
# Script 01 : Nettoyage des données & Gestion des Valeurs Manquantes
# Dataset   : NYC Yellow Taxi Trips - Juin 2020
# =============================================================================

# 0. Chargement des bibliothèques

library(tidyverse)   # dplyr, tidyr, ggplot2, readr, ...
library(lubridate)   # manipulation des dates
library(readxl)      # lecture Excel
library(janitor)     # nettoyage noms de colonnes
library(naniar)      # visualisation des NA
library(mice)        # imputation multiple
library(skimr)       # résumé rapide
library(knitr)       # tableaux



# 1. Chargement des données brutes
# Chargement des zones taxi
taxi_zone <- read_excel("data/raw/taxi_zone.xlsx")
View(taxi_zone) 

# Chargement du dataset principal (lecture partielle d'abord pour vérifier)
tripdata <- read_csv("data/raw/yellow_tripdata_2020-06.csv")
View(tripdata)

# 2. Exploration initiale

# Aperçu général
glimpse(tripdata)

# Résumé rapide avec skimr
skim_result <- skim(tripdata)
print(skim_result)

# Statistiques descriptives de base
str(tripdata)
head(tripdata)
summary(tripdata)


# 3. Diagnostic des Valeurs Manquantes
cat("\n========== DIAGNOSTIC DES NA ==========\n")

# Nombre de NA par colonne
na_summary <- tripdata %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(
    pct_na = round(n_na / nrow(tripdata) * 100, 2),
    has_na = n_na > 0
  ) %>%
  arrange(desc(n_na))

cat("\nRésumé des valeurs manquantes :\n")
print(na_summary)

# Visualisation des NA (gg_miss_var)
p_na <- gg_miss_var(tripdata, show_pct = TRUE) +
  labs(
    title    = "Valeurs manquantes par variable",
    subtitle = "NYC Yellow Taxi - Juin 2020",
    x        = "Variable",
    y        = "% de NA"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/01_missing_values.png", p_na, width = 10, height = 6, dpi = 150)
cat("Graphique NA sauvegardé : output/01_missing_values.png\n")

# Pattern de NA (combinaison de colonnes)
miss_pattern <- miss_var_summary(tripdata)
print(miss_pattern)

# 4. Nettoyage des données
cat("\n========== NETTOYAGE ==========\n")

tripdata_clean <- tripdata %>%
  
  # --- 4a. Renommer pour plus de clarté ---
  rename(
    vendor_id             = VendorID,
    pickup_datetime       = tpep_pickup_datetime,
    dropoff_datetime      = tpep_dropoff_datetime,
    rate_code_id          = RatecodeID,
    pu_location_id        = PULocationID,
    do_location_id        = DOLocationID
  ) %>%
  clean_names() %>%
  
  # --- 4b. Filtrer la période correcte (juin 2020) ---
  filter(
    pickup_datetime  >= as.POSIXct("2020-06-01"),
    pickup_datetime  <  as.POSIXct("2020-07-01"),
    dropoff_datetime >= as.POSIXct("2020-06-01"),
    dropoff_datetime <  as.POSIXct("2020-07-01")
  ) %>%
  
  # --- 4c. Supprimer les trajets impossibles ---
  filter(
    trip_distance   > 0,          # distance positive
    trip_distance   <= 200,        # pas de trajets aberrants (> 200 miles)
    fare_amount     > 0,           # tarif positif
    fare_amount     <= 500,        # tarif raisonnable
    total_amount    > 0,
    total_amount    <= 600,
    passenger_count >= 1,          # au moins 1 passager
    passenger_count <= 6,          # max 6 passagers
    !is.na(pu_location_id),        # zones valides
    !is.na(do_location_id)
  ) %>%
  
  # --- 4d. Durée du trajet ---
  mutate(
    trip_duration_min = as.numeric(difftime(dropoff_datetime, pickup_datetime,
                                            units = "mins")),
    .after = dropoff_datetime
  ) %>%
  filter(
    trip_duration_min > 0,         # durée positive
    trip_duration_min <= 360       # max 6 heures
  ) %>%
  
  # --- 4e. Variables temporelles ---
  mutate(
    pickup_hour    = hour(pickup_datetime),
    pickup_day     = wday(pickup_datetime, label = TRUE, abbr = FALSE, week_start = 1),
    pickup_date    = as_date(pickup_datetime),
    is_weekend     = pickup_day %in% c("samedi", "dimanche"),
    time_of_day    = case_when(
      pickup_hour >= 6  & pickup_hour < 12 ~ "Matin",
      pickup_hour >= 12 & pickup_hour < 18 ~ "Après-midi",
      pickup_hour >= 18 & pickup_hour < 23 ~ "Soir",
      TRUE                                 ~ "Nuit"
    ),
    time_of_day = factor(time_of_day, levels = c("Matin","Après-midi","Soir","Nuit"))
  ) %>%
  
  # --- 4f. Types de paiement et codes lisibles ---
  mutate(
    payment_label = case_when(
      payment_type == 1 ~ "Carte de crédit",
      payment_type == 2 ~ "Espèces",
      payment_type == 3 ~ "Aucun frais",
      payment_type == 4 ~ "Dispute",
      payment_type == 5 ~ "Inconnu",
      payment_type == 6 ~ "Voyage annulé",
      TRUE              ~ "Autre"
    ),
    vendor_label = case_when(
      vendor_id == 1 ~ "Creative Mobile Technologies",
      vendor_id == 2 ~ "VeriFone Inc.",
      TRUE           ~ "Autre"
    ),
    store_and_fwd_flag = if_else(store_and_fwd_flag == "Y", "Oui", "Non")
  ) %>%
  
  # --- 4g. Vitesse moyenne (mph) ---
  mutate(
    speed_mph = round(trip_distance / (trip_duration_min / 60), 2)
  ) %>%
  filter(speed_mph <= 100)   # filtrer vitesses aberrantes

# --- 4h. Jointure avec les zones géographiques ---
tripdata_clean <- tripdata_clean %>%
  left_join(
    taxi_zone %>% select(LocationID, Borough, Zone, service_zone) %>%
      rename(pu_location_id= LocationID,
             pu_borough     = Borough,
             pu_zone        = Zone,
             pu_service     = service_zone),
    by = "pu_location_id"
  ) %>%
  left_join(
    taxi_zone %>% select(LocationID, Borough, Zone, service_zone) %>%
      rename(do_location_id = LocationID,
             do_borough     = Borough,
             do_zone        = Zone,
             do_service     = service_zone),
    by = "do_location_id"
  )

cat(sprintf("données nettoyées : %s lignes × %d colonnes\n",
            format(nrow(tripdata_clean), big.mark = ","), ncol(tripdata_clean)))

# 5. Gestion des NA restants (Imputation)
cat("\n========== IMPUTATION DES NA RESTANTS ==========\n")

na_after <- colSums(is.na(tripdata_clean))
cat("NA restants après nettoyage :\n")
print(na_after[na_after > 0])

# Imputation par la médiane pour les variables numériques avec peu de NA
trips_final <- tripdata_clean %>%
  mutate(
    passenger_count      = if_else(is.na(passenger_count),
                                   median(passenger_count, na.rm = TRUE),
                                   passenger_count),
    congestion_surcharge = if_else(is.na(congestion_surcharge), 0,
                                   congestion_surcharge)
  )

# 6. Rapport de qualité des données
cat("\n========== RAPPORT DE QUALITÉ ==========\n")

n_raw    <- nrow(tripdata)
n_clean  <- nrow(trips_final)
n_suppr  <- n_raw - n_clean
pct_kept <- round(n_clean / n_raw * 100, 1)

rapport_qualite <- tibble(
  Métrique            = c("Observations brutes",
                          "Observations supprimées",
                          "Observations conservées",
                          "% conservé",
                          "Colonnes finales",
                          "NA résiduels totaux"),
  Valeur              = c(format(n_raw,   big.mark = ","),
                          format(n_suppr, big.mark = ","),
                          format(n_clean, big.mark = ","),
                          paste0(pct_kept, "%"),
                          ncol(trips_final),
                          sum(is.na(trips_final)))
)

print(rapport_qualite)

# 7. Sauvegarde des données nettoyées
saveRDS(trips_final, "data/clean/trips_clean.rds")
saveRDS(taxi_zone,       "data/clean/zones_clean.rds")
write_csv(trips_final, "data/clean/trips_clean.csv")


