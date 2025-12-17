#################### code utilisé
#### analyse variables quali
tab = read.table("data_planetes.csv", 
                 header = TRUE, 
                 sep = ",", 
                 stringsAsFactors = FALSE 
)
tab
# 1. On crée le tableau des effectifs trié
effectifs_methode <- sort(table(tab$detection_method), decreasing = TRUE)

# 2. On garde les 3 premiers
table_top3 <- effectifs_methode[1:3]

# 3. Renommage en Français
names(table_top3) <- c("Transit", "Vitesse Radiale", "Imagerie Directe")

# --- L'ASTUCE EST ICI ---
# Pour un graphique HORIZONTAL, on inverse l'ordre pour que le 1er soit en HAUT
table_top3_ordonne <- rev(table_top3)
couleurs_ordonnees <- rev(c("#E7298A", "#7570B3","#1B9E77" ))

# 5. Génération du graphique
png("barplot_top3_methodes.png", width = 800, height = 600)
par(mar=c(5, 12, 4, 2)) 

barplot(table_top3_ordonne,
        main = "Top 3 des Méthodes de Détection",
        xlab = "Nombre de Planètes",
        col = couleurs_ordonnees,
        horiz = TRUE,
        las = 1)

dev.off()



######----------- quali univarié planete
# Renommage des catégories en français (méthode simple)
effectifs_type <- table(tab$planet_type)
names(effectifs_type)[names(effectifs_type) == "Gas Giant"] <- "Géante Gazeuse"
names(effectifs_type)[names(effectifs_type) == "Neptune-like"] <- "Type Neptune"
names(effectifs_type)[names(effectifs_type) == "Super Earth"] <- "Super Terre"
names(effectifs_type)[names(effectifs_type) == "Terrestrial"] <- "Rocheuse"

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




######---------------------- 
#AFC

library(FactoMineR)
library(factoextra)


# 2. Création de la table de contingence complète
data <- table(tab$planet_type, tab$detection_method)

# 3. SÉLECTION : On ne garde que vos 3 méthodes
# Attention : il faut utiliser les noms exacts présents dans le fichier (en anglais)
methodes_a_garder <- c("Transit", "Radial Velocity", "Direct Imaging")

# On filtre le tableau pour ne garder que ces colonnes
data_top3 <- data[, methodes_a_garder]

# 4. Exécution de l'AFC sur ce sous-ensemble
res.ca <- CA(data_top3, graph = TRUE) 

# 5. Affichage des résultats
print(res.ca$eig)       # Les valeurs propres (inertie)
print(res.ca$row$cos2)  # La qualité de représentation des lignes (types de planètes)

# 6. Pour afficher le graphique biplot propre
fviz_ca_biplot(res.ca, repel = TRUE, title = "AFC : Types de Planètes vs Méthodes (Top 3)")




#########---------quanti quali masse rayon 
# On enlève les valeurs nulles ou négatives pour le log
tab_clean <- subset(tab, mass_jup > 0 & rayon_jup > 0 & distance>0 & luminosité>0)

# 2. Tracer le nuage de points (Données brutes)
plot(tab_clean$mass_jup, tab_clean$rayon_jup,
     main = "Relation Masse et Rayon (Brut)",
     xlab = "Masse (Jupiters)", 
     ylab = "Rayon (Jupiters)")
# Constat : On voit une courbe, pas une droite. Le modèle linéaire simple ne marchera pas.

# 3. Calculer la corrélation linéaire brute
cor(tab_clean$mass_jup, tab_clean$rayon_jup) 


# --- PASSAGE AUX LOGARITHMES (Comme Exo 4, étape 6) ---

# 4. Transformation des variables
u = log(tab_clean$mass_jup)   
v = log(tab_clean$rayon_jup) 

cor(u, v) 

plot(u, v,
     main = "Masse vs Rayon (unités de Jupiter)",
     xlab = "Masse", 
     ylab = "Rayon")


# NOUVELLE LIGNE : Ajout de la droite de régression (mod2) sur le graphique
abline(mod2, col="red", lwd=2)
# 5. Corrélation sur les logs
cor(u, v) 
# 0.92 forte corrélation positive

# 6. Modèle de régression sur les logs (Comme Exo 4, étape 7)
mod2 = lm(v ~ u)


# Afficher les coefficients
mod2$coefficients 
# D'après vos résultats : 
# (Intercept) = -0.1535
# u (pente)   = 0.3604
# Equation : log(Rayon) = 0.3604 * log(Masse) - 0.1535

# 7. Validation du modèle
summary(mod2) 
# Multiple R-squared = 0.857
# 85.7% de la variabilité du rayon est expliquée par la masse.
# On valide le modèle (> 80%).

