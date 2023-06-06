# Partie 1 : Importation des données

# On vérifie d'avoir installé les packages nécessaires
# install.packages("dplyr") 
# install.packages("readr")

library(dplyr)
library(readr)

# Importation du fichier CSV contenant les données des séismes
data <- read_csv("seismes_2014.csv")

# Partie 2 : Filtrer les données des cinq etats d'intérêt

# Définir les etats d'intérêt
etats <- c("California", "NewYork", "Texas", "Washington", "Illinois")

# Filtrer les données pour les séismes dans les etats souhaités
seismes <- filter(data, pays %in% etats)

# Afficher les premières lignes des données filtrées
head(seismes)

# Partie 3 : Analyse exploratoire des données

# Visualiser la répartition des séismes dans les différentes etats
library(ggplot2)

ggplot(seismes, aes(x = mag, fill = pays)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5) +
  labs(x = "Magnitude", y = "Nombre de séismes", fill = "États") +
  theme_minimal()

# Visualiser la répartition des séismes dans les différentes etats sans la californie
ggplot(filter(seismes, pays != "California"), aes(x = mag, fill = pays)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5) +
  labs(x = "Magnitude", y = "Nombre de séismes", fill = "États") +
  theme_minimal()

# Partie 4 : Division des données en groupes pour chaque etat

groupes <- split(seismes$mag, seismes$pays)

# Partie 5 : Test de Kruskal-Wallis pour comparer les distributions des magnitudes de séisme

stat_test <- kruskal.test(seismes$mag, seismes$pays)

# Afficher les résultats du test statistique
print(stat_test)


# Partie 6 : Interprétation des résultats du test statistique

alpha <- 0.05  # Niveau de significativité

if (stat_test$p.value < alpha) {
  cat("Les distributions des magnitudes de séisme sont significativement différentes entre les etats.\n")
  cat("On rejette l'hypothèse nulle (H0) : tous les etats ont la même vulnérabilité aux séismes.\n")
} else {
  cat("Les distributions des magnitudes de séisme ne sont pas significativement différentes entre les etats.\n")
  cat("On ne peut pas rejeter l'hypothèse nulle (H0) : tous les etats ont la même vulnérabilité aux séismes.\n")
}

# On cherche à savoir quels etats ont des distributions de magnitudes de séisme significativement différentes
# On effectue le test de Wilcoxon pour comparer les distributions des magnitudes de séisme entre chaque paire d'etats
for (i in 1:(length(groupes))) {
  for (j in (i + 1):length(groupes)) {
    stat_test <- wilcox.test(groupes[[i]], groupes[[j]])
    if (stat_test$p.value < alpha) {
      cat("Les distributions des magnitudes de séisme sont significativement différentes entre", names(groupes)[i], "et", names(groupes)[j], "\n")
    }
  }
}

# On cherche à savoir quel etat a eu le plus de seismes
# On calcule le nombre de seismes par etat
nb_seismes <- sapply(groupes, length)

# On affiche le nombre de seismes par etat
print(nb_seismes)

# Partie 7 : Comparaison des distributions des seismes entre les etats des États-Unis

# On défini les etats des États-Unis
etats_US <- c("Alaska", "California", "Hawaii", "Nevada", "Washington", "Arizona", "Idaho", "Montana", "Oregon", "Utah", "Wyoming", "Colorado", "NewMexico", "NorthDakota", "SouthDakota", "Nebraska", "Kansas", "Oklahoma", "Texas", "Arkansas", "Louisiana", "Missouri", "Iowa", "Minnesota", "Wisconsin", "Illinois", "Indiana", "Kentucky", "Michigan", "Ohio", "Tennessee", "Alabama", "Florida", "Georgia", "Mississippi", "NorthCarolina", "SouthCarolina", "Virginia", "WestVirginia", "Delaware", "Maryland", "Pennsylvania", "NewJersey", "NewYork", "Connecticut", "Maine", "Massachusetts", "NewHampshire", "RhodeIsland", "Vermont")

# On filtre les données pour les etats des États-Unis
seismes_US <- filter(data, pays %in% etats_US)

# On divise les données en groupes pour chaque etat
groupes_US <- split(seismes_US$mag, seismes_US$pays)

# On effectue le test de student pour comparer les distributions des magnitudes de séisme aux États-Unis
stat_test_US <- t.test(seismes_US$mag, seismes$mag)

# Afficher les résultats du test statistique
print(stat_test_US)