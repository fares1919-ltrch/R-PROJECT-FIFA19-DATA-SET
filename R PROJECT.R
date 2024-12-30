# Partie 1 : Importation et exploration initiale des données
# Description : Cette section couvre l'importation du jeu de données, l'affichage de ses premières lignes (head), ses dernières lignes (tail), et des sélections clés.

# Charger les bibliothèques nécessaires
library(readr)
library(dplyr)
library(tidyr)

# Charger le jeu de données
# Assurez-vous que le fichier "data_minimized.csv" est dans le même répertoire de travail
file_path <- "E:/Desktop/LANG-R/R PROJECT/data_minimized.csv"
data <- read_csv(file_path)

# Afficher les premières lignes
data_head <- head(data)
print("Premières lignes du jeu de données :")
print(data_head)

# Afficher les dernières lignes
data_tail <- tail(data)
print("Dernières lignes du jeu de données :")
print(data_tail)

# Afficher toutes les colonnes et les dimensions
data_dimensions <- dim(data)
print(paste("Dimensions du jeu de données :", data_dimensions[1], "lignes et", data_dimensions[2], "colonnes"))

# Sélection des joueurs par poste (Position)
print("Sélection des joueurs par poste :")
unique_positions <- unique(data$Position)
print("Postes disponibles :")
print(unique_positions)

# Partie 2 : Manipulation des données avec dplyr et tidyr
# Description : Cette section utilise les bibliothèques dplyr et tidyr pour manipuler et transformer les données de manière efficace.

selected_position <- data %>% filter(Position == "ST")
print("Joueurs sélectionnés pour le poste ST :")
print(head(selected_position$Name))

# Sélection des joueurs du Real Madrid
real_madrid_players <- data %>% filter(Club == "Real Madrid")
print("Joueurs du Real Madrid :")
print(real_madrid_players$Name)

# Analyse des joueurs selon leurs postes majeurs
position_analysis <- data %>% group_by(Position) %>% summarise(Average_Age = mean(Age, na.rm = TRUE), Count = n())
print("Analyse par poste (moyenne d'âge et nombre de joueurs) :")
print(head(position_analysis))

# Filtrer les lignes basées sur une condition (exemple : garder les joueurs ayant un âge > 25)
filtered_data <- data %>% filter(Age > 25)
print("Données filtrées (Age > 25) :")
print(head(filtered_data))

# Sélectionner des colonnes spécifiques (exemple : 'Name' et 'Age')
selected_data <- data %>% select(Name, Age)
print("Colonnes sélectionnées :")
print(head(selected_data))

# Calculer l'âge moyen par club et l'afficher dans une colonne unique
average_age_per_club <- data %>%
  group_by(Club) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE)) %>%
  ungroup()

# Afficher les résultats
print("Âge moyen par club :")
print(average_age_per_club)

# Pivot Wider - Chaque club devient une colonne
wider_data <- average_age_per_club %>%
  pivot_wider(names_from = Club, values_from = Average_Age)

print("Données après pivot_wider :")
print(wider_data)

# Pivot Long - Revenir à un format long
long_data <- wider_data %>%
  pivot_longer(cols = everything(), names_to = "Club", values_to = "Average_Age")

print("Données après pivot_longer :")
print(long_data)

# Partie 3 : Visualisation des données avec ggplot2
# Description : Création de visualisations simples pour comprendre les données.

# Charger la bibliothèque ggplot2
library(ggplot2)

# Exemple 1 : Histogramme de l'âge
ggplot(data, aes(x = Age)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  labs(title = "Histogramme de l'âge", x = "Age", y = "Nombre de joueurs")

# Exemple 2 : Boxplot pour visualiser l'âge par position
ggplot(data, aes(x = Position, y = Age, fill = Position)) + 
  geom_boxplot() + 
  labs(title = "Diagramme à boîtes de l'âge par position", x = "Position", y = "Âge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

# Boxplot pour visualiser la valeur par position
ggplot(data, aes(x = Position, y = Value, fill = Position)) + 
  geom_boxplot() + 
  labs(title = "Diagramme à boîtes de la valeur par position", x = "Position", y = "Valeur") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

# Bar plot pour visualiser l'âge moyen par position
avg_age_by_position <- data %>%
  group_by(Position) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE))

ggplot(avg_age_by_position, aes(x = Position, y = Average_Age, fill = Position)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Âge moyen par position", x = "Position", y = "Âge moyen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

library(plotly)
# Interactive boxplot pour visualiser l'âge par position
p1 <- ggplot(data, aes(x = Position, y = Age, fill = Position)) + 
  geom_boxplot() + 
  labs(title = "Diagramme à boîtes de l'âge par position", x = "Position", y = "Âge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Convertir ggplot en plot interactif avec plotly
interactive_p1 <- ggplotly(p1)
interactive_p1
# Convertir ggplot en plot interactif avec plotly
interactive_p1 <- ggplotly(p1)
interactive_p1
# Partie 4 : Ajout de parties supplémentaires
# Description : Analyse statistique de base, comme le calcul de la moyenne et de la médiane.

# Calcul de la moyenne et de la médiane de l'âge
age_mean <- mean(data$Age, na.rm = TRUE)
age_median <- median(data$Age, na.rm = TRUE)
print(paste("Moyenne de l'âge :", age_mean))
print(paste("Médiane de l'âge :", age_median))



