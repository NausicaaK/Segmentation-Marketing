# Segmentation-Marketing
L’objectif de cette segmentation est d’identifier des groupes distincts de membres du programme Loyalty en fonction de leur engagement et de leurs habitudes d’achat. Cette approche vise à mieux comprendre le comportement des clients afin d’adapter les stratégies marketing et d’optimiser la gestion du programme de fidélisation

# Charger les bibliothèques
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(lubridate)
library(cluster)
library(ggcorrplot)

# Importation des fichiers CSV
members  <- read.csv("~/Desktop/MEMBERS_DIM.csv", stringsAsFactors = FALSE)
rewards <- read.csv("~/Desktop/REWARD_FACT.csv", stringsAsFactors = FALSE)
transactions <- read.csv("~/Desktop/TRANSACTION_FACT.csv", stringsAsFactors = FALSE)

# Vérification initiale
summary(members)
summary(Rewards)
summary(Transaction)

```

Members- Nettoyage 


# Supprimer les valeurs aberrantes dans AGE
members <- members %>%
  filter(AGE >= 18 & AGE <= 100)

summary(members$AGE)

# gérer les valeurs manquantes
median_age <- median(members$AGE, na.rm = TRUE)
members$AGE <- ifelse(is.na(members$AGE), median_age, members$AGE)

colSums(is.na(members))


```

Transactions - Nettoyage / Transformation


colSums(is.na(transactions))

# Supprimer catégorie 8
transactions <- transactions %>%
  filter(RETAILER != 'CATEGORY_8')

transactions <- transactions %>%
  mutate(RETAILER = ifelse(RETAILER == "" | is.na(RETAILER), "0", RETAILER))

# Nettoyage des valeurs manquantes dans TRANSACTION_FACT
transactions <- transactions %>%
  mutate(
    REWARD_POINTS_EARNED = ifelse(is.na(REWARD_POINTS_EARNED), 0, REWARD_POINTS_EARNED),
    CASH_BACK_POINTS_EARNED = ifelse(is.na(CASH_BACK_POINTS_EARNED), 0, CASH_BACK_POINTS_EARNED)
  )

# Agrégation des transactions et des récompenses par membre

transactions_summary <- transactions %>%
  group_by(MEMBER_ID) %>%
  summarise(
    total_transactions = n(),
    total_spent = sum(AMOUNT_SPENT, na.rm = TRUE),
    avg_spent = mean(AMOUNT_SPENT, na.rm = TRUE),
    unique_retailers = n_distinct(RETAILER)
  )



Rewards -Transformation



rewards_summary <- rewards %>%
  group_by(MEMBER_ID) %>%
  summarise(
    total_points_earned = sum(POINTS_REDEEMED, na.rm = TRUE),
    total_redemptions = sum(REDEMPTIONS, na.rm = TRUE),
    avg_items_redeemed = mean(NUMBER_ITEMS_REDEEMED, na.rm = TRUE)
  )


# Fusion des tables
merged_data <- members %>%
  left_join(transactions_summary, by = "MEMBER_ID") %>%
  left_join(rewards_summary, by = "MEMBER_ID")


summary(merged_data)
colSums(is.na(merged_data))

# Remplacement des NA par 0
merged_data[is.na(merged_data)] <- 0

library(ggcorrplot)

library(ggcorrplot)

# Sélectionner uniquement les variables numériques
numeric_vars <- merged_data %>%
  select(where(is.numeric))

# Calculer la matrice de corrélation
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Visualiser la matrice de corrélation avec ggcorrplot
ggcorrplot(cor_matrix, 
           method = "square",  # Correction ici
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"), 
           title = "Matrice de Corrélation des Variables Numériques",
           ggtheme = theme_minimal())



```

Optimisation - échantillon - normalisation 

```{r}
# Sélection des variables optimisées
variables_optimisées <- c("total_spent", "avg_spent", "unique_retailers", "total_points_earned")

# Réduction des valeurs extrêmes avec l'IQR (Intervalle Interquartile)
reduce_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1

  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  x <- ifelse(x < lower_bound, lower_bound, x)
  x <- ifelse(x > upper_bound, upper_bound, x)

  return(x)
}

# Appliquer la réduction des outliers sur les variables sélectionnées
segmentation_data <- merged_data %>%
  select(MEMBER_ID, all_of(variables_optimisées)) %>%  # Garde MEMBER_ID et les variables optimisées
  mutate(across(all_of(variables_optimisées), reduce_outliers))  # Applique la réduction des outliers

summary(segmentation_data)

segmentation_data[is.na(segmentation_data)] <- 0

# Échantillonnage
set.seed(123) 
sample_indices <- sample(nrow(segmentation_data), size = 0.2 * nrow(segmentation_data))
segmentation_sample <- segmentation_data[sample_indices, ]

# Avant d'enlever MEMBER_ID, on garde une copie
segmentation_sample_with_id <- segmentation_data[sample_indices, ] 

# Supprimer MEMBER_ID
segmentation_sample <- segmentation_sample %>%
  select(-MEMBER_ID)  # Supprime MEMBER_ID avant clustering


```


CLUSTERING - PCA - METHODE DU COUDE


# Normalisation des données échantillonnées
clustering_data_scaled <- scale(segmentation_data_scaled)

summary(clustering_data_scaled) 
clustering_data_scaled[is.na(clustering_data_scaled)] <- 0

# Appliquer la PCA
pca_result <- prcomp(clustering_data_scaled, center = TRUE, scale. = TRUE)

# Visualiser la variance expliquée par chaque composante
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

#Convertir le résultat de la PCA en un dataframe
pca_data <- as.data.frame(pca_result$x)

# Sélectionner le nombre de composantes principales expliquant au moins 80% de la variance
num_components <- which(cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) >= 0.90)[1]
pca_data <- as.data.frame(pca_result$x[, 1:num_components])

# Vérification de la distribution des valeurs après PCA
summary(pca_data)

# Définition des paramètres
set.seed(123)  
max_k <- 8  
wss <- numeric(max_k)  

# Boucle pour calculer l'inertie intra-cluster pour chaque valeur de K
for (k in 2:max_k) {  
  kmeans_result <- tryCatch(
    {kmeans(pca_data, centers = k, nstart = 50, iter.max = 500, algorithm = "MacQueen") },
    warning = function(w) {
      message(paste("Avertissement pour K =", k, ": ", w$message))
      return(NULL) },
    error = function(e) {
      message(paste("Erreur pour K =", k, ": ", e$message))
      return(NULL) }  )
  
  if (!is.null(kmeans_result)) {wss[k] <- kmeans_result$tot.withinss   } else {wss[k] <- NA }}

# Suppression des valeurs NA avant la visualisation
elbow_plot <- data.frame(K = 2:max_k, WSS = wss[2:max_k])

# Tracé de la méthode du coude
ggplot(elbow_plot, aes(x = K, y = WSS)) +
  geom_line() + 
  geom_point() +
  ggtitle("Détermination du nombre optimal de clusters avec la méthode du coude") +
  xlab("Nombre de Clusters (K)") +
  ylab("Inertie intra-cluster (WSS)") +
  theme_minimal()


# Définition du nombre optimal de clusters (choisir la meilleure valeur de K)
optimal_k <- 4  # Remplace par la valeur optimale trouvée

# Appliquer K-means sur les données PCA
set.seed(123)
final_kmeans <- kmeans(pca_data, centers = optimal_k, nstart = 50, iter.max = 500)

# Ajouter les labels des clusters aux données PCA
pca_data$cluster <- as.factor(final_kmeans$cluster)

# Visualisation des clusters avec ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Visualisation des clusters après K-means",
       x = "Première composante principale (PC1)",
       y = "Deuxième composante principale (PC2)") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "pink", "violet"))  

table(pca_data$cluster)


```


CARACTERISTIQUE DES CLUSTERS


segmentation_sample_with_id$cluster <- pca_data$cluster  # Ajouter les clusters aux données originales

summary_by_cluster <- segmentation_sample_with_id %>%
  group_by(cluster) %>%
  summarise(across(c(total_spent, avg_spent, unique_retailers, total_points_earned), 
                   \(x) mean(x, na.rm = TRUE)))


print(summary_by_cluster)


```


ANALYSE DES VARIABLES SOCIO-DÉMOGRAPHIQUES 

# Ajout des variables socio-démographiques depuis `members`
segmentation_sample_with_id <- segmentation_sample_with_id %>%
  left_join(members %>% select(MEMBER_ID, AGE, GENDER, PROV, LANGUAGE), by = "MEMBER_ID")

summary_by_cluster <- segmentation_sample_with_id %>%
  group_by(cluster) %>%
  summarise(
    avg_total_spent = mean(total_spent, na.rm = TRUE),
    avg_avg_spent = mean(avg_spent, na.rm = TRUE),
    avg_unique_retailers = mean(unique_retailers, na.rm = TRUE),
    avg_total_points_earned = mean(total_points_earned, na.rm = TRUE),
    avg_age = mean(AGE, na.rm = TRUE),  # ✅ Moyenne d'âge par cluster
    gender_distribution = list(table(GENDER)),  # ✅ Distribution des genres
    prov_distribution = list(table(PROV)),  # ✅ Distribution des provinces
    language_distribution = list(table(LANGUAGE))  # ✅ Distribution des langues
  )

view(summary_by_cluster)

# Répartition Genre
segmentation_sample_with_id %>%
  group_by(cluster, GENDER) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = as.factor(cluster), y = percentage, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Répartition du Genre par Cluster", x = "Cluster", y = "Pourcentage") +
  theme_minimal()

# Répartition des Provinces par Cluster
segmentation_sample_with_id %>%
  group_by(cluster, PROV) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = as.factor(cluster), y = percentage, fill = PROV)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Répartition des Provinces par Cluster", x = "Cluster", y = "Pourcentage") +
  theme_minimal()

# Répartition des langues par cluster  
segmentation_sample_with_id %>%
  group_by(cluster, LANGUAGE) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = as.factor(cluster), y = percentage, fill = LANGUAGE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Répartition des Langues par Cluster", x = "Cluster", y = "Pourcentage") +
  theme_minimal()

# Répartion de l'Âge par cluster
ggplot(segmentation_sample_with_id, aes(x = as.factor(cluster), y = AGE, fill = as.factor(cluster))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Répartition de l'Âge par Cluster",
       x = "Cluster",
       y = "Âge") +
  theme_minimal()
