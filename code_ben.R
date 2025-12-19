
#----Type de méthode de détection-------
tab = read.table("data_planetes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE )
effectifs_methode <- sort(table(tab$detection_method), decreasing = TRUE)
table_top3 <- effectifs_methode[1:3] # garde seulement les 3 plus grosses méthode de détections 
names(table_top3) <- c("Transit", "Vitesse Radiale", "Imagerie Directe")
table_top3_ordonne <- rev(table_top3) # inverse le sens
couleurs_ordonnees <- rev(c("#E7298A", "#7570B3","#1B9E77" ))
par(mar=c(5, 12, 4, 2)) 
# affiche graph
barplot(table_top3_ordonne,main = "Top 3 des Méthodes de Détection",xlab = "Nombre de Planètes",
        col = couleurs_ordonnees,horiz = TRUE,las = 1)



#----Type de planète-------
# Renommage des catégories en français 
effectifs_type <- table(tab$planet_type)
names(effectifs_type)[names(effectifs_type) == "Gas Giant"] <- "Géante Gazeuse"
names(effectifs_type)[names(effectifs_type) == "Neptune-like"] <- "Type Neptune"
names(effectifs_type)[names(effectifs_type) == "Super Earth"] <- "Super Terre"
names(effectifs_type)[names(effectifs_type) == "Terrestrial"] <- "Rocheuse"
effectifs_tries <- sort(effectifs_type, decreasing = FALSE)
couleurs_foncees <- c("#D95F02", "#1B9E77", "#7570B3", "#E7298A")
par(mar=c(5, 15, 4, 2) + 0.1) 
barplot(effectifs_tries,main = "Effectifs par Type de Planète",xlab = "Nombre de Planètes",
        xlim = c(0, 2000), col = couleurs_foncees, horiz = TRUE, las = 1) 


#----AFC-------
library(FactoMineR)
library(factoextra)
data <- table(tab$planet_type, tab$detection_method)
#SÉLECTION : On ne garde que vos 3 méthodes
methodes_a_garder <- c("Transit", "Radial Velocity", "Direct Imaging")
# On filtre le tableau pour ne garder que ces colonnes
data_top3 <- data[, methodes_a_garder]
#Exécution de l'AFC sur ce sous-ensemble
res.ca <- CA(data_top3, graph = TRUE) 
#Affichage des résultats
print(res.ca$eig)       # Les valeurs propres (inertie)
print(res.ca$row$cos2)  # La qualité de représentation des lignes (types de planètes)
#Pour afficher le graphique biplot propre
fviz_ca_biplot(res.ca, repel = TRUE, title = "AFC : Types de Planètes vs Méthodes (Top 3)")



#----Masse/Rayon-------
# On enlève les valeurs nulles ou négatives pour le log
tab_clean <- subset(tab, mass_jup > 0 & rayon_jup > 0 & distance>0 & luminosité>0)
#Transformation des variables
u = log(tab_clean$mass_jup)   
v = log(tab_clean$rayon_jup) 
plot(u, v,main = "Masse vs Rayon (unités de Jupiter)",xlab = "Masse", ylab = "Rayon")
abline(mod2, col="red", lwd=2)
cor(u, v) # 0.92 forte corrélation positive
mod2 = lm(v ~ u)
# Afficher les coefficients
mod2$coefficients # Equation : log(Rayon) = 0.3604 * log(Masse) - 0.1535
summary(mod2) # On valide le modèle (> 80%).
