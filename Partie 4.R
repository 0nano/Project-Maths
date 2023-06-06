# Création d'une page web avec Shiny avec threejs

library(shiny)
library(threejs)

# On lit les données de seismes
seismes <- read.csv("seismes_2014.csv", header = TRUE, sep = ",")

# On retire les lignes avec des valeurs manquantes
seismes <- na.omit(seismes)
# On retire les lignes avec des valeurs négatives
seismes <- seismes[seismes$mag > 0,]

# On créé une échelle de couleur pour les seismes par rapport à leur magnitude
palette <- c("green", "yellow", "purple", "red", "blue", "white", "black", "orange")

# On créé un objet globejs pour être afficher dans l'interface Shiny
ui <- fluidPage(
  globeOutput("globe")
)

server <- function(input, output) {
  output$globe <- renderGlobe({
    # On créé un globejs avec les données de seismes
    globejs(
      lon = seismes$lon, 
      lat = seismes$lat, 
      value = (seismes$mag^2.75)/2,
      color = palette[seismes$mag+1])
  })
}

shinyApp(ui = ui, server = server)