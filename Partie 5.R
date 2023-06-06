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

# Partie 7 : Tests post-hoc pour identifier les différences significatives entre les villes

# Partie 8 : Analyse des résultats post-hoc

if (stat_test$p.value < alpha) {
  # Effectuer un test Kruskal-Wallis
  kruskal_result <- kruskal.test(Magnitude ~ Ville, data = seismes)
  
  # Effectuer des tests post-hoc pour identifier les paires de villes significativement différentes
  library(pgirmess)
  
  posthoc_results <- pairwise.wilcox.test(seismes$Magnitude, seismes$Ville, p.adjust.method = "bonferroni")
  
  # Afficher les paires de villes significativement différentes
  significant_pairs <- posthoc_results$p.value %>%
    as.data.frame() %>%
    rownames_to_column(var = "Pair") %>%
    filter(Pair != "NA") %>%
    mutate(p.adjust = p.adjust(p.value, method = "bonferroni"))
  
  cat("Les paires de villes significativement différentes dans les distributions des magnitudes de séisme sont :\n")
  print(significant_pairs)
  
  # Partie 9 : Analyse des résultats et conclusion
  
  if (nrow(significant_pairs) > 0) {
    cat("Selon les résultats des tests post-hoc, certaines villes présentent des différences significatives dans les distributions des magnitudes de séisme.\n")
    
    # Trouver la ville avec la magnitude de séisme la plus basse
    ville_moins_vulnerable <- seismes %>% 
      group_by(Ville) %>% 
      summarise(Moyenne_Magnitude = mean(Magnitude)) %>% 
      filter(Moyenne_Magnitude == min(Moyenne_Magnitude))
      
    cat("La ville la moins vulnérable aux séismes, selon les tests, est :", ville_moins_vulnerable$Ville, "\n")
    cat("L'hypothèse nulle (H0) est rejetée : toutes les villes n'ont pas la même vulnérabilité aux séismes.\n")
  } else {
    cat("Aucune paire de villes ne présente de différence significative dans les distributions des magnitudes de séisme.\n")
    cat("Toutes les villes ont des vulnérabilités similaires aux séismes.\n")
    cat("L'hypothèse nulle (H0) est validée : toutes les villes ont la même vulnérabilité aux séismes.\n")
  }
} else {
  cat("Les distributions des magnitudes de séisme ne diffèrent pas significativement entre les villes.\n")
  cat("Toutes les villes ont des vulnérabilités similaires aux séismes.\n")
  cat("L'hypothèse nulle (H0) est validée : toutes les villes ont la même vulnérabilité aux séismes.\n")
}