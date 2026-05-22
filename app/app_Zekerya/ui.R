# =============================================================================
# PROJET R - Master SSD | Semestre 2 - 2025/2026
# Application Shiny – NYC Yellow Taxi Dashboard
# Fichier : app/ui.R
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(DT)

# Interface Utilisateur

ui <- dashboardPage(
  skin = "black",

  # ── HEADER ────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/8/83/Yellow_taxi_cabs.jpg",
               height = "40px", style = "margin-right:8px;border-radius:4px;"),
      "NYC Taxi Dashboard"
    ),
    titleWidth = 280
  ),

  # ── SIDEBAR ───────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Vue Générale",     tabName = "overview",   icon = icon("chart-line")),
      menuItem("⏱ Analyse Temporelle", tabName = "temporal",  icon = icon("clock")),
      menuItem("🗺 Géographie",        tabName = "geo",        icon = icon("map-marker-alt")),
      menuItem("💳 Paiements & Tarifs",tabName = "payments",   icon = icon("dollar-sign")),
      menuItem("📊 Données",           tabName = "data",       icon = icon("table")),
      menuItem("ℹ Info",              tabName = "about",       icon = icon("info-circle"))
    ),

    hr(),
    tags$div(
      style = "padding: 10px 15px;",
      tags$h5(tags$b("🔧 Filtres globaux"), style = "color:#ccc; margin-bottom:10px;"),

      # Sélection Borough
      pickerInput(
        inputId  = "sel_borough",
        label    = "Borough de départ",
        choices  = c("Tous", "Manhattan", "Brooklyn", "Queens",
                      "Bronx", "Staten Island", "EWR"),
        selected = "Tous",
        multiple = FALSE,
        options  = list(style = "btn-primary btn-sm")
      ),

      # Moment de la journée
      checkboxGroupButtons(
        inputId  = "sel_tod",
        label    = "Moment de la journée",
        choices  = c("Matin", "Après-midi", "Soir", "Nuit"),
        selected = c("Matin", "Après-midi", "Soir", "Nuit"),
        status   = "primary",
        size     = "sm",
        checkIcon = list(
          yes = icon("check"),
          no  = icon("remove")
        )
      ),

      # Plage de distance
      sliderInput(
        inputId = "sel_dist",
        label   = "Distance (miles)",
        min     = 0, max = 30, value = c(0, 30), step = 0.5
      ),

      # Plage de tarif
      sliderInput(
        inputId = "sel_fare",
        label   = "Tarif ($)",
        min     = 0, max = 200, value = c(0, 200), step = 5
      ),

      # Mode de paiement
      pickerInput(
        inputId  = "sel_payment",
        label    = "Mode de paiement",
        choices  = c("Tous","Carte de crédit","Espèces","Aucun frais"),
        selected = "Tous",
        options  = list(style = "btn-info btn-sm")
      ),

      # Bouton reset
      actionButton("btn_reset", "🔄 Réinitialiser", 
                   class = "btn-warning btn-sm", width = "100%")
    )
  ),

  # ── BODY ──────────────────────────────────────────────────────────────────
  dashboardBody(
    # CSS personnalisé
    tags$head(tags$style(HTML("
      .main-header .logo { font-weight: 700; font-size: 16px; }
      .value-box .value  { font-size: 28px; font-weight: 700; }
      .value-box .header { font-size: 12px; }
      .box-header .box-title { font-weight: 700; font-size: 14px; }
      .content-wrapper, .main-sidebar { min-height: 100vh; }
      body { background-color: #1a1a2e; color: #eee; }
      .box { border-radius: 8px; }
      .small-box h3 { font-size: 35px; }
    "))),

    tabItems(

      # ===================================================================
      # TAB 1 : VUE GÉNÉRALE
      # ===================================================================
      tabItem(
        tabName = "overview",

        # KPI Boxes
        fluidRow(
          valueBoxOutput("kpi_total_trips",   width = 3),
          valueBoxOutput("kpi_avg_fare",      width = 3),
          valueBoxOutput("kpi_avg_distance",  width = 3),
          valueBoxOutput("kpi_avg_duration",  width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_avg_tip",       width = 3),
          valueBoxOutput("kpi_avg_speed",     width = 3),
          valueBoxOutput("kpi_pct_card",      width = 3),
          valueBoxOutput("kpi_pct_manhattan", width = 3)
        ),

        # Graphiques principaux
        fluidRow(
          box(
            title  = "📈 Distribution des distances",
            width  = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_dist_hist", height = 300)
          ),
          box(
            title  = "💵 Distribution des tarifs",
            width  = 6, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_fare_hist", height = 300)
          )
        ),
        fluidRow(
          box(
            title  = "🏙 Trajets par Borough",
            width  = 6, status = "danger", solidHeader = TRUE,
            plotlyOutput("plot_borough_bar", height = 300)
          ),
          box(
            title  = "💳 Modes de paiement",
            width  = 6, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_payment_pie", height = 300)
          )
        )
      ),

      # ===================================================================
      # TAB 2 : ANALYSE TEMPORELLE
      # ===================================================================
      tabItem(
        tabName = "temporal",
        fluidRow(
          box(
            title       = "⏰ Profil horaire – Nombre de trajets",
            width       = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_hourly", height = 300)
          )
        ),
        fluidRow(
          box(
            title  = "🗓 Heatmap heure × jour",
            width  = 8, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_heatmap", height = 350)
          ),
          box(
            title  = "📅 Trajets par jour de semaine",
            width  = 4, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_dayofweek", height = 350)
          )
        ),
        fluidRow(
          box(
            title  = "📆 Évolution quotidienne",
            width  = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_daily", height = 280)
          )
        )
      ),

      # ===================================================================
      # TAB 3 : GÉOGRAPHIE
      # ===================================================================
      tabItem(
        tabName = "geo",
        fluidRow(
          box(
            title  = "🗺 Carte interactive des départs",
            width  = 8, status = "primary", solidHeader = TRUE,
            leafletOutput("map_nyc", height = 500)
          ),
          box(
            title  = "📍 Top 15 zones de départ",
            width  = 4, status = "danger", solidHeader = TRUE,
            plotlyOutput("plot_top_zones", height = 500)
          )
        ),
        fluidRow(
          box(
            title  = "↔ Flux Borough → Borough",
            width  = 12, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_od_matrix", height = 350)
          )
        )
      ),

      # ===================================================================
      # TAB 4 : PAIEMENTS & TARIFS
      # ===================================================================
      tabItem(
        tabName = "payments",
        fluidRow(
          box(
            title  = "💰 Tarif par moment de la journée",
            width  = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_fare_tod", height = 320)
          ),
          box(
            title  = "📏 Distance vs Tarif",
            width  = 6, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_scatter", height = 320)
          )
        ),
        fluidRow(
          box(
            title  = "🏙 Tarif moyen par Borough",
            width  = 6, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_fare_borough", height = 300)
          ),
          box(
            title  = "🎁 Pourboire par mode de paiement",
            width  = 6, status = "danger", solidHeader = TRUE,
            plotlyOutput("plot_tip", height = 300)
          )
        )
      ),

      # ===================================================================
      # TAB 5 : DONNÉES
      # ===================================================================
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title  = "📊 Table des données filtrées",
            width  = 12, status = "primary", solidHeader = TRUE,
            downloadButton("btn_download", "⬇ Télécharger CSV", class = "btn-success"),
            br(), br(),
            DTOutput("data_table")
          )
        )
      ),

      # ===================================================================
      # TAB 6 : À PROPOS
      # ===================================================================
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title  = "ℹ À propos de ce projet",
            width  = 12, status = "info", solidHeader = TRUE,
            tags$div(
              style = "padding: 20px; font-size: 15px;",
              tags$h4(tags$b("🚕 NYC Yellow Taxi Dashboard")),
              tags$p("Ce dashboard a été développé dans le cadre du ",
                     tags$b("Projet R – Master SSD, Semestre 2, 2025/2026"),
                     " au Département de Math-Info, FST."),
              tags$h5(tags$b("📦 Données")),
              tags$ul(
                tags$li("Source : ", tags$a("NYC TLC Open Data",
                         href = "https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page",
                         target = "_blank")),
                tags$li("Période : Juin 2020"),
                tags$li("Variables : 18+ colonnes (temporelles, spatiales, financières)")
              ),
              tags$h5(tags$b("🛠 Technologies")),
              tags$ul(
                tags$li("R / Shiny pour le dashboard"),
                tags$li("tidyverse pour la manipulation de données"),
                tags$li("plotly pour les graphiques interactifs"),
                tags$li("leaflet pour la cartographie"),
                tags$li("DT pour les tables interactives")
              ),
              tags$h5(tags$b("📋 Structure du Projet")),
              tags$pre(
"projet_taxi_R/
├── data/               # Données brutes et nettoyées
├── scripts/
│   ├── 01_cleaning.R   # Nettoyage & NA
│   ├── 02_analysis.R   # Analyse statistique
│   └── 03_visualization.R  # Visualisations
├── app/
│   ├── ui.R            # Interface Shiny
│   └── server.R        # Logique Shiny
├── output/             # Graphiques exportés
└── reports/            # Rapports Rmarkdown"
              )
            )
          )
        )
      )
    )
  )
)
