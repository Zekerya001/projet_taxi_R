# =============================================================================
# PROJET R - Master SSD | Semestre 2 - 2025/2026
# Lanceur de l'application Shiny
# Fichier : app/app.R (alternative unique-fichier)
# =============================================================================
# Ce fichier est une version combinée de ui.R + server.R
# Vous pouvez lancer l'app depuis le terminal avec :
#   Rscript -e "shiny::runApp('app/', port=3838)"
# Ou depuis RStudio : ouvrez app.R et cliquez "Run App"
# =============================================================================

# Charger ui et server séparément
source("ui.R")
source("server.R")

shiny::shinyApp(ui = ui, server = server)
