# Partie 1 : Importation des données

# On vérifie d'avoir installé les packages nécessaires
 install.packages("dplyr") 
 install.packages("readr")

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

ggplot(seismes, aes(x = Ville)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Répartition des séismes par ville") +
  xlab("Ville") +
  ylab("Nombre de séismes")

# Partie 4 : Division des données en groupes pour chaque ville

groupes <- split(seismes$Magnitude, seismes$Ville)

# Partie 5 : Test de Kruskal-Wallis pour comparer les distributions des magnitudes de séisme

stat_test <- kruskal.test(seismes$Magnitude, seismes$Ville)

# Afficher les résultats du test statistique
print(stat_test)
