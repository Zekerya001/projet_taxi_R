²# =============================================================================
# PROJET R - Master SSD | Semestre 2 - 2025/2026
# Script 03 : Visualisations & Cartographie
# Dataset   : NYC Yellow Taxi Trips - Juin 2020
# =============================================================================

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)   # combiner plusieurs ggplots
library(viridis)     # palettes de couleurs
library(leaflet)     # cartes interactives
library(sf)          # données spatiales
library(htmlwidgets) # sauvegarder leaflet en HTML
library(plotly)      # graphiques interactifs
library(RColorBrewer)
library(patchwork)



cat(" Bibliothèques chargées\n")

# Thème personnalisé
theme_taxi <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, color = "#1a1a2e"),
    plot.subtitle    = element_text(size = 11, color = "#555555"),
    plot.caption     = element_text(size = 9, color = "#999999"),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold")
  )

theme_set(theme_taxi)

# Couleurs principales
COLORS_BOROUGH <- c(
  "Manhattan"    = "#E63946",
  "Brooklyn"     = "#457B9D",
  "Queens"       = "#2A9D8F",
  "Bronx"        = "#E9C46A",
  "Staten Island"= "#A8DADC",
  "EWR"          = "#6D6875"
)

# 0. Chargement des données
trips <- readRDS("data/clean/trips_analyzed.rds")
zones <- readRDS("data/clean/zones_clean.rds")

cat(sprintf(" %s trajets chargés\n", format(nrow(trips), big.mark = ",")))

# 1. Distribution des Variables Principales
cat("\n--- Graphiques de distribution ---\n")

# Histogramme des distances
p_dist <- ggplot(trips %>% filter(trip_distance <= 20), 
                 aes(x = trip_distance)) +
  geom_histogram(bins = 50, fill = "#457B9D", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(trip_distance)), 
             color = "#E63946", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(trips$trip_distance) + 0.5, y = Inf,
           label = sprintf("Médiane: %.1f mi", median(trips$trip_distance)),
           vjust = 2, hjust = 0, color = "#E63946", fontface = "bold") +
  scale_x_continuous(labels = function(x) paste0(x, " mi")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution des distances de trajet",
       subtitle = "NYC Yellow Taxi – Juin 2020 (trajets ≤ 20 miles)",
       x = "Distance (miles)", y = "Nombre de trajets",
       caption = "Source : NYC TLC")

# Histogramme des tarifs
p_fare <- ggplot(trips %>% filter(fare_amount <= 80),
                 aes(x = fare_amount)) +
  geom_histogram(bins = 50, fill = "#E9C46A", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(fare_amount)),
             color = "#E63946", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(trips$fare_amount) + 1, y = Inf,
           label = sprintf("Médiane: $%.1f", median(trips$fare_amount)),
           vjust = 2, hjust = 0, color = "#E63946", fontface = "bold") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution des tarifs",
       x = "Tarif de base ($)", y = "Nombre de trajets")

# Histogramme de la durée
p_dur <- ggplot(trips %>% filter(trip_duration_min <= 90),
                aes(x = trip_duration_min)) +
  geom_histogram(bins = 50, fill = "#2A9D8F", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(trip_duration_min)),
             color = "#E63946", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution des durées de trajet",
       x = "Durée (minutes)", y = "Nombre de trajets")

# Combiner avec patchwork
p_distributions <- (p_dist | p_fare) / p_dur +
  plot_annotation(
    title    = "Distributions des variables principales – NYC Yellow Taxi Juin 2020",
    theme    = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("output/03_distributions.png", p_distributions, 
       width = 14, height = 10, dpi = 150)
cat(" output/03_distributions.png\n")

# 2. Analyse Temporelle
cat("\n--- Graphiques temporels ---\n")

# Profil horaire
by_hour <- trips %>%
  group_by(pickup_hour) %>%
  summarise(n_trips = n(), avg_fare = mean(fare_amount), .groups = "drop")

p_hourly <- ggplot(by_hour, aes(x = pickup_hour, y = n_trips)) +
  geom_area(fill = "#457B9D", alpha = 0.3) +
  geom_line(color = "#457B9D", linewidth = 1.2) +
  geom_point(color = "#E63946", size = 2.5) +
  scale_x_continuous(breaks = 0:23, labels = paste0(0:23, "h")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Profil horaire des trajets",
       subtitle = "Nombre de trajets par heure – Juin 2020",
       x = "Heure de la journée", y = "Nombre de trajets")

# Tarif moyen par heure
p_fare_hour <- ggplot(by_hour, aes(x = pickup_hour, y = avg_fare)) +
  geom_col(fill = "#E9C46A", alpha = 0.9) +
  scale_x_continuous(breaks = 0:23, labels = paste0(0:23, "h")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Tarif moyen par heure",
       x = "Heure", y = "Tarif moyen ($)")

p_temporal <- p_hourly / p_fare_hour +
  plot_annotation(
    title = "Analyse temporelle – Profil horaire des taxis de New York",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("output/04_profil_horaire.png", p_temporal, width = 14, height = 10, dpi = 150)
cat(" output/04_profil_horaire.png\n")

# Heatmap heure × jour
heatmap_data <- trips %>%
  filter(!is.na(pickup_day)) %>%
  group_by(pickup_day, pickup_hour) %>%
  summarise(n_trips = n(), .groups = "drop")

p_heatmap <- ggplot(heatmap_data, aes(x = pickup_hour, y = pickup_day, fill = n_trips)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, name = "Nbre de\ntrajets") +
  scale_x_continuous(breaks = seq(0, 23, 3), labels = paste0(seq(0, 23, 3), "h")) +
  labs(
    title    = "Heatmap : Activité taxi par heure et jour de la semaine",
    subtitle = "NYC Yellow Taxi – Juin 2020",
    x        = "Heure", y        = "Jour"
  )

ggsave("output/05_heatmap_heure_jour.png", p_heatmap, width = 12, height = 7, dpi = 150)
cat(" output/05_heatmap_heure_jour.png\n")

# Évolution quotidienne
by_date <- trips %>%
  group_by(pickup_date) %>%
  summarise(n_trips = n(), avg_fare = mean(fare_amount), .groups = "drop")

p_daily <- ggplot(by_date, aes(x = pickup_date, y = n_trips)) +
  geom_area(fill = "#2A9D8F", alpha = 0.3) +
  geom_line(color = "#2A9D8F", linewidth = 1.2) +
  geom_smooth(method = "loess", se = TRUE, color = "#E63946",
              linetype = "dashed", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Évolution quotidienne du nombre de trajets",
    subtitle = "Juin 2020 – NYC Yellow Taxi",
    x        = "Date", y = "Nombre de trajets"
  )

ggsave("output/06_evolution_quotidienne.png", p_daily, width = 12, height = 6, dpi = 150)
cat("output/06_evolution_quotidienne.png\n")

# 3. Analyse par Borough
cat("\n--- Graphiques par Borough ---\n")

by_borough <- trips %>%
  filter(!is.na(pu_borough)) %>%
  group_by(pu_borough) %>%
  summarise(
    n_trips    = n(),
    avg_fare   = mean(fare_amount),
    avg_dist   = mean(trip_distance),
    avg_tip    = mean(tip_amount),
    .groups    = "drop"
  ) %>%
  arrange(desc(n_trips))

# Barres horizontales — nombre de trajets
p_borough_n <- ggplot(by_borough,
                      aes(x = n_trips, y = reorder(pu_borough, n_trips),
                          fill = pu_borough)) +
  geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) +
  geom_text(aes(label = comma(n_trips)), hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = COLORS_BOROUGH) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Nombre de trajets par Borough de départ",
       x = "Nombre de trajets", y = NULL)

# Tarif moyen par borough
p_borough_fare <- ggplot(by_borough,
                          aes(x = avg_fare, y = reorder(pu_borough, avg_fare),
                              fill = pu_borough)) +
  geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) +
  geom_text(aes(label = dollar(round(avg_fare, 2))), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = COLORS_BOROUGH) +
  scale_x_continuous(labels = scales::dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Tarif moyen par Borough de départ",
       x = "Tarif moyen ($)", y = NULL)

p_borough_combined <- p_borough_n | p_borough_fare
ggsave("output/07_analyse_borough.png", p_borough_combined, 
       width = 14, height = 6, dpi = 150)
cat(" output/07_analyse_borough.png\n")

# 4. Modes de Paiement
percent <- scales::percent
p_payment <- trips %>%
  filter(payment_label %in% c("Carte de crédit","Espèces","Aucun frais")) %>%
  count(payment_label) %>%
  mutate(
    pct   = n / sum(n),
    label = paste0(payment_label, "\n", percent(pct, accuracy = 0.1))
  ) %>%
  ggplot(aes(x = "", y = pct, fill = payment_label)) +
  geom_col(width = 1, color = "white", linewidth = 1.5) +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Carte de crédit" = "#457B9D",
                                "Espèces"          = "#E9C46A",
                                "Aucun frais"      = "#2A9D8F")) +
  theme_void(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5)) +
  labs(title = "Répartition des modes de paiement",
       subtitle = "NYC Yellow Taxi – Juin 2020")

ggsave("output/08_paiements.png", p_payment, width = 8, height = 7, dpi = 150)
cat(" output/08_paiements.png\n")

# 5. Boxplots – Tarif par moment de la journée
p_tod <- trips %>%
  filter(!is.na(time_of_day)) %>%
  ggplot(aes(x = time_of_day, y = fare_amount, fill = time_of_day)) +
  geom_violin(trim = TRUE, alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA, linewidth = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 60)) +
  labs(
    title    = "Distribution des tarifs par moment de la journée",
    subtitle = "Violin plot + Boxplot – NYC Yellow Taxi Juin 2020",
    x        = "Moment de la journée", y = "Tarif ($)",
    fill     = NULL
  )

ggsave("output/09_tarif_moment_journee.png", p_tod, width = 10, height = 7, dpi = 150)
cat(" output/09_tarif_moment_journee.png\n")

# 6. Relation Distance ~ Tarif (scatter)
p_scatter <- trips %>%
  sample_n(min(5000, nrow(trips))) %>%
  ggplot(aes(x = trip_distance, y = fare_amount, color = pu_borough)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#1a1a2e",
              linewidth = 1.5, linetype = "dashed") +
  scale_color_manual(values = COLORS_BOROUGH, name = "Borough départ") +
  scale_x_continuous(limits = c(0, 20), labels = function(x) paste0(x, " mi")) +
  scale_y_continuous(limits = c(0, 80), labels = dollar) +
  labs(
    title    = "Relation entre distance et tarif de trajet",
    subtitle = "Échantillon aléatoire de 5 000 trajets",
    x        = "Distance (miles)", y = "Tarif ($)"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

ggsave("output/10_distance_tarif.png", p_scatter, width = 11, height = 7, dpi = 150)
cat("output/10_distance_tarif.png\n")

# 7. Top 10 Zones de Départ et d'Arrivée
top_pu <- trips %>%
  filter(!is.na(pu_zone)) %>%
  count(pu_zone, pu_borough, sort = TRUE) %>%
  head(10)

top_do <- trips %>%
  filter(!is.na(do_zone)) %>%
  count(do_zone, do_borough, sort = TRUE) %>%
  head(10)

p_top_pu <- ggplot(top_pu, aes(x = n, y = reorder(pu_zone, n), fill = pu_borough)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.2) +
  scale_fill_manual(values = COLORS_BOROUGH, name = "Borough") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 10 zones de départ", x = "Nombre de départs", y = NULL)

p_top_do <- ggplot(top_do, aes(x = n, y = reorder(do_zone, n), fill = do_borough)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.2) +
  scale_fill_manual(values = COLORS_BOROUGH, name = "Borough") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 10 zones d'arrivée", x = "Nombre d'arrivées", y = NULL)

p_top_zones <- p_top_pu / p_top_do
ggsave("output/11_top_zones.png", p_top_zones, width = 13, height = 12, dpi = 150)
cat("output/11_top_zones.png\n")

# 8. Cartographie avec Leaflet
cat("\n--- Cartographie interactive (Leaflet) ---\n")

# Coordonnées approximatives des centres de zones (ex. avec centroïdes fictifs)
# En production : utiliser le shapefile TLC + st_centroid()
# Ici on utilise un proxy basé sur les données de trips par LocationID

zone_activity <- trips %>%
  filter(!is.na(pu_location_id)) %>%
  group_by(pu_location_id) %>%
  summarise(
    n_trips    = n(),
    avg_fare   = round(mean(fare_amount), 2),
    avg_dist   = round(mean(trip_distance), 2),
    .groups    = "drop"
  ) %>%
  left_join(zones, by = c("pu_location_id" = "LocationID"))

# Coordonnées approximatives des boroughs (centroïdes)
borough_coords <- tibble(
  Borough   = c("Manhattan","Brooklyn","Queens","Bronx","Staten Island","EWR"),
  lat       = c(40.7831,   40.6782,  40.7282, 40.8448, 40.5795,        40.6895),
  lon       = c(-73.9712, -73.9442, -73.7949, -73.8648, -74.1502,      -74.1745)
)

zone_map_data <- zone_activity %>%
  left_join(borough_coords, by = "Borough") %>%
  # ajouter un léger bruit pour séparer les points
  mutate(
    lat = lat + runif(n(), -0.03, 0.03),
    lon = lon + runif(n(), -0.03, 0.03)
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# Palette de couleurs selon le volume
pal_leaflet <- colorNumeric(
  palette = "YlOrRd",
  domain  = zone_map_data$n_trips
)

# Carte leaflet
# 1. On prépare proprement les données et le rayon à l'avance pour éviter les bugs dans leaflet
zone_map_cleaned <- zone_map_data %>%
  mutate(
    # Calcule le rayon ici (évite le bug du ~scales::rescale)
    radius_calc = scales::rescale(sqrt(n_trips), to = c(4, 25))
  )

# 2. Création de la carte
map_taxi <- leaflet(zone_map_cleaned) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = -74.006, lat = 40.7128, zoom = 11) %>%
  addCircleMarkers(
    lng         = ~lon,
    lat         = ~lat,
    radius      = ~radius_calc, # <-- On utilise la colonne calculée proprement
    color       = ~pal_leaflet(n_trips),
    fillColor   = ~pal_leaflet(n_trips),
    fillOpacity = 0.75,
    stroke      = TRUE,
    weight      = 1,
    popup       = ~paste0(
      "<b>Zone :</b> ", Zone, "<br>",  
      "<b>Borough :</b> ", Borough, "<br>",
      "<b>Trajets :</b> ", format(n_trips, big.mark = ","), "<br>",
      "<b>Tarif moyen :</b> $", round(avg_fare, 2), "<br>", # Petit arrondi pour le propre
     "<b>Distance moy. :</b> ", round(avg_dist, 2), " miles"
  )
 ) %>%
addLegend(
    position = "bottomright",
    pal      = pal_leaflet,
    values   = ~n_trips,
    title    = "Nombre de départs",
    labFormat = labelFormat(big.mark = " ")
  ) %>%
  addControl(
    # Ajout d'un fond transparent/sombre pour le titre afin qu'il soit lisible sur la carte
    html     = "<div style='background: rgba(0,0,0,0.5); padding: 8px; border-radius: 5px;'><h4 style='margin:0;color:#fff;'>NYC Yellow Taxi – Juin 2020<br><small style='color:#ccc;'>Zones de départ</small></h4></div>",
    position = "topleft"
  )

saveWidget(map_taxi, "output/12_carte_interactive.html", selfcontained = TRUE)
cat("output/12_carte_interactive.html\n")

# 9. Cartographie SF (carte choroplèthe par borough)
 if (file.exists("data/taxi_zones.shp")) {
   nyc_sf <- st_read("data/taxi_zones.shp") %>%
     st_transform(crs = 4326)

   nyc_sf_joined <- nyc_sf %>%
     left_join(zone_activity, by = c("LocationID" = "pu_location_id"))

   p_choro <- ggplot(nyc_sf_joined) +
     geom_sf(aes(fill = n_trips), color = "white", linewidth = 0.1) +
     scale_fill_viridis_c(option = "magma", labels = comma, na.value = "grey80",
                          name = "Nbre de départs") +
     labs(title = "Intensité des départs par zone",
          subtitle = "NYC Yellow Taxi – Juin 2020",
         caption = "Source : NYC TLC") +
     theme_void(base_size = 12) +
     theme(plot.title = element_text(face = "bold", size = 16))
   ggsave("output/13_choroplethe.png", p_choro, width = 12, height = 10, dpi = 150)
}



cat("\n🗺️  Note : Pour la carte choroplèthe complète,\n")
cat("    téléchargez le shapefile TLC et décommentez la section 9.\n")

# 10. Dashboard résumé (image unique)
p_summary <- (p_dist | p_fare) / (p_hourly | p_borough_n) +
  plot_annotation(
    title    = "NYC Yellow Taxi – Tableau de bord résumé – Juin 2020",
    subtitle = "Données nettoyées | Source : NYC TLC Open Data",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#555")
    )
  )

ggsave("output/00_dashboard_resume.png", p_summary, width = 16, height = 12, dpi = 150)
cat(" output/00_dashboard_resume.png\n")

cat("\n Script 03 terminé – Tous les graphiques générés dans output/\n")
