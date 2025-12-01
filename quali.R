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

# 3. Représentation graphique : Diagramme en bâtons (Barplot)
barplot(table(tab$planet_type), 
        main = "Répartition des types de planètes",
        ylab = "Effectifs",
        col = "orange",
        las = 2) # las=2 pour lire les étiquettes verticales

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

# 3. Barplot
# On augmente la marge gauche (mar) car les noms des méthodes sont longs
par(mar=c(5, 12, 4, 2)) 
barplot(sort(effectifs_methode), 
        main = "Méthodes de détection",
        horiz = TRUE, # Barres horizontales pour lire les noms
        col = "lightblue",
        las = 1)

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
