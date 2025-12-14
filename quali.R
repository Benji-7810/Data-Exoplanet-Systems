#### analyse variables quali
tab = read.table("data_planetes.csv", 
                 header = TRUE, 
                 sep = ",", 
                 stringsAsFactors = FALSE 
)
tab


# 1. Tableau des effectifs 
table(tab$planet_type)


# 2. Tableau des fréquences 
(round(prop.table(table(tab$planet_type)), 3)) # Arrondi à 3 décimales pour la lisibilité

tab <- read.csv("data_planetes.csv", header = TRUE, stringsAsFactors = FALSE)



tab <- read.csv("data_planetes.csv", header = TRUE, stringsAsFactors = FALSE)

# Renommage des catégories en français (méthode simple)
effectifs_type <- table(tab$planet_type)
names(effectifs_type)[names(effectifs_type) == "Gas Giant"] <- "Géante Gazeuse"
names(effectifs_type)[names(effectifs_type) == "Neptune-like"] <- "Type Neptune"
names(effectifs_type)[names(effectifs_type) == "Super Earth"] <- "Super Terre"
names(effectifs_type)[names(effectifs_type) == "Terrestrial"] <- "Terrestre"

effectifs_tries <- sort(effectifs_type, decreasing = FALSE)

# Définition d'une palette de couleurs douces (Pastel)
couleurs_foncees <- c("#D95F02", "#1B9E77", "#7570B3", "#E7298A")

# Génération du graphique avec xlim = 2000
png("barplot_types_francais_doux.png", width = 800, height = 600)
par(mar=c(5, 15, 4, 2) + 0.1) 
barplot(effectifs_tries,
        main = "Effectifs par Type de Planète",
        xlab = "Nombre de Planètes",
        xlim = c(0, 2000), 
        col = couleurs_foncees, 
        horiz = TRUE, 
        las = 1) 
dev.off()
# 4. Représentation graphique : Diagramme circulaire (Pie chart)
# Commande explicite du cours pour les variables qualitatives
pie(table(tab$planet_type), 
    main = "Répartition des types (Camembert)",
    col = rainbow(length(table(tab$planet_type)))) # Couleurs variées


#-----------------------------------#
# 1. Tableau des effectifs
effectifs_methode <- table(tab$detection_method)
print(sort(effectifs_methode, decreasing = TRUE)) # Tri pour y voir plus clair

# 2. Fréquences
freq_methode <- prop.table(effectifs_methode)
print(round(freq_methode, 4))


effectifs_methode <- table(tab$detection_method)

# Créer une palette de couleurs pour mettre en évidence les dominants
# Les dominants (Transit et Radial Velocity) sont souvent en haut du tri
couleurs <- ifelse(names(effectifs_methode) %in% c("Transit", "Radial Velocity"), 
                   "darkblue", # Couleur foncée pour les dominants
                   "lightgray") # Couleur discrète pour les autres

# Génération du Barplot final
par(mar=c(5, 15, 4, 2)) # Augmentation de la marge gauche
barplot(sort(effectifs_methode), 
        main = "Répartition des Méthodes de Détection d'Exoplanètes",
        xlab = "Nombre de Détections",
        col = sort(couleurs), # Appliquer les couleurs triées
        horiz = TRUE, 
        las = 1,
        cex.names = 0.8)


#-------------------------------#
# 1. Tableau des effectifs
effectifs_annee <- table(tab$year_class)
print(effectifs_annee)

# 2. Fréquences
freq_annee <- prop.table(effectifs_annee)
print(round(freq_annee, 2)) # Pourcentage (0.10 = 10%)

# 3. Barplot
par(mar=c(5, 4, 4, 2)) # On remet les marges standard
barplot(effectifs_annee, 
        main = "Découvertes par période",
        col = c("gray", "darkgray", "black"),
        ylim = c(0, max(effectifs_annee)*1.2)) # Un peu d'espace en haut

# 4. Pie chart
# Très adapté ici car il y a peu de catégories (3 classes)
pie(effectifs_annee, 
    main = "Proportion des découvertes par période",
    col = c("gray", "blue", "green"),
    labels = paste(names(effectifs_annee), "\n", round(freq_annee*100, 1), "%"))








