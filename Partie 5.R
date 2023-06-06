# Partie 1 : Importation des données

# On vérifie d'avoir installé les packages nécessaires
# install.packages("dplyr") 
# install.packages("readr")

library(dplyr)
library(readr)

# Importation du fichier CSV contenant les données des séismes
data <- read_csv("seismes_2014.csv")

# Partie 2 : Filtrer les données des cinq villes d'intérêt

# Définir les villes d'intérêt
etats <- c("California", "NewYork", "Texas", "Washington", "Illinois")

# Filtrer les données pour les séismes dans les etats souhaités
seismes <- filter(data, pays %in% etats)

# Afficher les premières lignes des données filtrées
head(seismes)

# Partie 3 : Analyse exploratoire des données

# Visualiser la répartition des séismes dans les différentes villes
library(ggplot2)

ggplot(seismes, aes(x = mag, fill = pays)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5) +
  labs(x = "Magnitude", y = "Nombre de séismes", fill = "Ville") +
  theme_minimal()

# Partie 4 : Division des données en groupes pour chaque ville

groupes <- split(seismes$mag, seismes$pays)

# Partie 5 : Test de Kruskal-Wallis pour comparer les distributions des magnitudes de séisme

stat_test <- kruskal.test(seismes$mag, seismes$pays)

# Afficher les résultats du test statistique
print(stat_test)


# Partie 6 : Interprétation des résultats du test statistique

alpha <- 0.05  # Niveau de significativité

if (stat_test$p.value < alpha) {
  cat("Les distributions des magnitudes de séisme sont significativement différentes entre les villes.\n")
  cat("On rejette l'hypothèse nulle (H0) : toutes les villes ont la même vulnérabilité aux séismes.\n")
} else {
  cat("Les distributions des magnitudes de séisme ne sont pas significativement différentes entre les villes.\n")
  cat("On ne peut pas rejeter l'hypothèse nulle (H0) : toutes les villes ont la même vulnérabilité aux séismes.\n")
}

