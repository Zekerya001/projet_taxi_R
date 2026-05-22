# =============================================================================
# PROJET R - Master SSD | Semestre 2 - 2025/2026
# Script 02 : Analyse Statistique & Descriptive
# Dataset   : NYC Yellow Taxi Trips - Juin 2020 (données nettoyées)
# =============================================================================

library(tidyverse)
library(lubridate)
library(moments)      # skewness, kurtosis
library(corrplot)     # matrice de corrélation
library(ggcorrplot)   # corrélation élégante
library(pastecs)      # stat.desc
library(knitr)
library(scales)       # formatage des axes
library(dplyr)
library(readr)

cat("Bibliothèques chargées\n")

# 0. Chargement des données nettoyées
trips  <- readRDS("data/clean/trips_clean.rds")
zones  <- readRDS("data/clean/zones_clean.rds")

cat(sprintf(" %s trajets chargés\n", format(nrow(trips), big.mark = ",")))

# 1. Statistiques Descriptives Générales
cat("\n========== STATISTIQUES DESCRIPTIVES ==========\n")

vars_num <- c("trip_distance", "trip_duration_min", "fare_amount",
              "tip_amount", "total_amount", "speed_mph", "passenger_count")

desc_stats <- trips %>%
  select(all_of(vars_num)) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(
    n        = n(),
    mean     = round(mean(value, na.rm = TRUE), 3),
    median   = round(median(value, na.rm = TRUE), 3),
    sd       = round(sd(value, na.rm = TRUE), 3),
    min      = round(min(value, na.rm = TRUE), 3),
    max      = round(max(value, na.rm = TRUE), 3),
    q25      = round(quantile(value, 0.25, na.rm = TRUE), 3),
    q75      = round(quantile(value, 0.75, na.rm = TRUE), 3),
    skewness = round(skewness(value, na.rm = TRUE), 3),
    kurtosis = round(kurtosis(value, na.rm = TRUE), 3),
    .groups  = "drop"
  )

cat("\nTable des statistiques descriptives :\n")
print(desc_stats)
write_csv(desc_stats, "output/desc_stats.csv")

# 2. Analyse par Borough (quartier)
cat("\n========== ANALYSE PAR BOROUGH ==========\n")

by_borough_pu <- trips %>%
  filter(!is.na(pu_borough)) %>%
  group_by(borough = pu_borough) %>%
  summarise(
    n_trips        = n(),
    avg_distance   = round(mean(trip_distance), 2),
    avg_duration   = round(mean(trip_duration_min), 2),
    avg_fare       = round(mean(fare_amount), 2),
    avg_total      = round(mean(total_amount), 2),
    avg_tip        = round(mean(tip_amount), 2),
    pct_card       = round(mean(payment_type == 1) * 100, 1),
    .groups        = "drop"
  ) %>%
  arrange(desc(n_trips))

cat("\nStatistiques par borough (départ) :\n")
print(by_borough_pu)
write_csv(by_borough_pu, "output/stats_by_borough.csv")

# 3. Analyse Temporelle
cat("\n========== ANALYSE TEMPORELLE ==========\n")

# Par heure
by_hour <- trips %>%
  group_by(pickup_hour) %>%
  summarise(
    n_trips      = n(),
    avg_fare     = round(mean(fare_amount), 2),
    avg_distance = round(mean(trip_distance), 2),
    avg_duration = round(mean(trip_duration_min), 2),
    avg_speed    = round(mean(speed_mph), 2),
    .groups      = "drop"
  )

# Par jour de la semaine
by_day <- trips %>%
  filter(!is.na(pickup_day)) %>%
  group_by(pickup_day) %>%
  summarise(
    n_trips      = n(),
    avg_fare     = round(mean(fare_amount), 2),
    avg_distance = round(mean(trip_distance), 2),
    .groups      = "drop"
  )

# Par date (évolution dans le mois)
by_date <- trips %>%
  group_by(pickup_date) %>%
  summarise(
    n_trips      = n(),
    avg_fare     = round(mean(fare_amount), 2),
    avg_distance = round(mean(trip_distance), 2),
    .groups      = "drop"
  )

cat("Heures de pointe (top 5) :\n")
print(by_hour %>% arrange(desc(n_trips)) %>% head(5))

cat("\nJours de la semaine :\n")
print(by_day)

write_csv(by_hour, "output/stats_by_hour.csv")
write_csv(by_day,  "output/stats_by_day.csv")
write_csv(by_date, "output/stats_by_date.csv")

# 4. Analyse des Paiements
cat("\n========== ANALYSE DES PAIEMENTS ==========\n")

by_payment <- trips %>%
  filter(!is.na(payment_label)) %>%
  group_by(payment_label) %>%
  summarise(
    n_trips    = n(),
    pct        = round(n() / nrow(trips) * 100, 1),
    avg_fare   = round(mean(fare_amount), 2),
    avg_tip    = round(mean(tip_amount), 2),
    avg_total  = round(mean(total_amount), 2),
    .groups    = "drop"
  ) %>%
  arrange(desc(n_trips))

cat("\nRépartition par mode de paiement :\n")
print(by_payment)
write_csv(by_payment, "output/stats_by_payment.csv")

# 5. Analyse des Distances et Tarifs
cat("\n========== ANALYSE DISTANCE & TARIF ==========\n")

# Catégories de distance
trips <- trips %>%
  mutate(
    distance_cat = case_when(
      trip_distance < 1   ~ "< 1 mile",
      trip_distance < 3   ~ "1–3 miles",
      trip_distance < 5   ~ "3–5 miles",
      trip_distance < 10  ~ "5–10 miles",
      TRUE                ~ "> 10 miles"
    ),
    distance_cat = factor(distance_cat, levels = c("< 1 mile","1–3 miles",
                                                    "3–5 miles","5–10 miles",
                                                    "> 10 miles"))
  )

by_distance_cat <- trips %>%
  group_by(distance_cat) %>%
  summarise(
    n_trips    = n(),
    pct        = round(n() / nrow(trips) * 100, 1),
    avg_fare   = round(mean(fare_amount), 2),
    avg_tip    = round(mean(tip_amount), 2),
    avg_speed  = round(mean(speed_mph), 2),
    .groups    = "drop"
  )

cat("\nRépartition par catégorie de distance :\n")
print(by_distance_cat)
write_csv(by_distance_cat, "output/stats_by_distance.csv")

# 6. Matrice de Corrélation
cat("\n========== MATRICE DE CORRÉLATION ==========\n")

corr_data <- trips %>%
  select(trip_distance, trip_duration_min, fare_amount, tip_amount,
         total_amount, speed_mph, passenger_count, pickup_hour) %>%
  drop_na()

corr_matrix <- cor(corr_data)
cat("\nMatrice de corrélation :\n")
print(round(corr_matrix, 3))

# Sauvegarde graphique
png("output/02_correlation_matrix.png", width = 900, height = 800, res = 120)
corrplot(
  corr_matrix,
  method   = "color",
  type     = "upper",
  order    = "hclust",
  tl.cex   = 0.9,
  tl.col   = "black",
  addCoef.col = "black",
  number.cex  = 0.7,
  col      = colorRampPalette(c("#D73027","#FFFFFF","#4575B4"))(200),
  title    = "Matrice de Corrélation – NYC Taxi Juin 2020",
  mar      = c(0, 0, 2, 0)
)
dev.off()
cat(" Matrice de corrélation sauvegardée : output/02_correlation_matrix.png\n")

# 7. Tests Statistiques
cat("\n========== TESTS STATISTIQUES ==========\n")

# Test : Différence de tarif week-end vs semaine
t_test_weekend <- t.test(
  fare_amount ~ is_weekend,
  data = trips %>% filter(!is.na(is_weekend))
)

cat("\nTest t – Tarif week-end vs semaine :\n")
cat(sprintf("  t = %.3f, p-value = %.4f\n",
            t_test_weekend$statistic, t_test_weekend$p.value))
cat(sprintf("  Moy. semaine = $%.2f | Moy. week-end = $%.2f\n",
            t_test_weekend$estimate[1], t_test_weekend$estimate[2]))

if (t_test_weekend$p.value < 0.05) {
  cat("  ✅ Différence statistiquement significative (α=0.05)\n")
} else {
  cat("  ❌ Pas de différence significative (α=0.05)\n")
}

# ANOVA : Tarif par borough
anova_borough <- aov(fare_amount ~ pu_borough, data = trips %>% filter(!is.na(pu_borough)))
cat("\nANOVA – Tarif par borough :\n")
print(summary(anova_borough))

# Régression linéaire simple : Tarif ~ Distance
lm_simple <- lm(fare_amount ~ trip_distance, data = trips)
cat("\nRégression linéaire – Tarif ~ Distance :\n")
print(summary(lm_simple))

# Régression multiple
lm_multi <- lm(fare_amount ~ trip_distance + trip_duration_min + passenger_count,
               data = trips)
cat("\nRégression multiple – Tarif ~ Distance + Durée + Passagers :\n")
print(summary(lm_multi))

# Sauvegarde des modèles
saveRDS(lm_multi, "output/model_fare.rds")

# 8. Résumé Exécutif
cat("\n========== RÉSUMÉ EXÉCUTIF ==========\n")

resume <- tibble(
  Indicateur = c(
    "Total trajets (nettoyés)",
    "Durée moyenne (min)",
    "Distance moyenne (miles)",
    "Tarif moyen ($)",
    "Pourboire moyen ($)",
    "Vitesse moyenne (mph)",
    "% Paiement carte",
    "% Trajets Manhattan",
    "Heure de pointe principale",
    "Jour le plus actif"
  ),
  Valeur = c(
    format(nrow(trips), big.mark = ","),
    round(mean(trips$trip_duration_min, na.rm = TRUE), 1),
    round(mean(trips$trip_distance, na.rm = TRUE), 2),
    paste0("$", round(mean(trips$fare_amount, na.rm = TRUE), 2)),
    paste0("$", round(mean(trips$tip_amount, na.rm = TRUE), 2)),
    round(mean(trips$speed_mph, na.rm = TRUE), 1),
    paste0(round(mean(trips$payment_type == 1, na.rm = TRUE) * 100, 1), "%"),
    paste0(round(mean(trips$pu_borough == "Manhattan", na.rm = TRUE) * 100, 1), "%"),
    paste0(by_hour %>% arrange(desc(n_trips)) %>% slice(1) %>% pull(pickup_hour), "h"),
    as.character(by_day %>% arrange(desc(n_trips)) %>% slice(1) %>% pull(pickup_day))
  )
)

print(resume)
write_csv(resume, "output/resume_executif.csv")

# Sauvegarde du dataset enrichi
saveRDS(trips, "data/clean/trips_analyzed.rds")

cat("\n Script 02 terminé avec succès !\n")
