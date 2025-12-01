#### analyse variables quanti
tab = read.table("data_planetes.csv", 
                 header = TRUE, 
                 sep = ",", 
                 stringsAsFactors = FALSE 
)
tab

#----------------------------------------------#
# 1. Résumé numérique
print("--- Résumé : Masse (en fonction de Jupiters) ---")
summary(tab$mass_jup)

# 2. Histogramme (Distribution)
hist(tab$mass_jup, 
     main = "Distribution de la Masse ", 
     xlab = "Masse (Jupiters)", 
     col = "lightblue", 
     breaks = 50) # On augmente les barres pour plus de détails

# 3. Boxplot (Valeurs extrêmes)
boxplot(tab$mass_jup, 
        main = "Boxplot de la Masse", 
        col = "lightblue",
        horizontal = TRUE) # Plus lisible à l'horizontale



#----------------------------------------------#
# 1. Résumé
print("--- Résumé : Rayon (en fonction de Jupiters) ---")
summary(tab$rayon_jup)

# 2. Histogramme
hist(tab$rayon_jup, 
     main = "Distribution du Rayon", 
     xlab = "Rayon (Jupiters)", 
     col = "lightgreen", 
     breaks = 30)

# 3. Boxplot
boxplot(tab$rayon_jup, 
        main = "Boxplot du Rayon", 
        col = "lightgreen",
        horizontal = TRUE)

#----------------------------------------------#
print("--- Résumé : Période Orbitale (Jours) ---")
summary(tab$orbital_period)

# Histogramme Logarithmique (car l'échelle est trop grande)
hist(log10(tab$orbital_period), 
     main = "Distribution Période Orbitale (Log10)", 
     xlab = "Log10(Jours) - (0 = 1 jour, 2 = 100 jours, 3 = 1000 jours...)", 
     col = "pink", 
     breaks = 30)

boxplot(log10(tab$orbital_period), 
        main = "Boxplot Période (Log10)", 
        col = "pink",
        horizontal = TRUE)


#----------------------------------------------#
print("--- Résumé : Distance à l'étoile (UA) ---")
summary(tab$orbital_radius)

# On zoome sur les planètes "proches" (< 5 UA, comme notre système solaire interne)
hist(tab$orbital_radius[tab$orbital_radius < 5], 
     main = "Distribution Distance à l'étoile (Zoom < 5 UA)", 
     xlab = "Distance (UA) - (1 = Terre)", 
     col = "wheat", 
     breaks = 20)
abline(v = 1, col = "blue", lwd = 2, lty = 2) # Repère de la Terre

boxplot(log10(tab$orbital_radius), 
        main = "Boxplot Distance Etoile (Log10)", 
        col = "wheat", 
        horizontal = TRUE)


#----------------------------------------------#
print("--- Résumé : Excentricité ---")
summary(tab$eccentricity)

hist(tab$eccentricity, 
     main = "Distribution de l'Excentricité", 
     xlab = "Excentricité (0 = Cercle parfait)", 
     col = "lavender", 
     breaks = 20)

boxplot(tab$eccentricity, 
        main = "Boxplot Excentricité", 
        col = "lavender", 
        horizontal = TRUE)

#----------------------------------------------#
print("--- Résumé : Magnitude (Luminosité) ---")
summary(tab$luminosité)

hist(tab$luminosité, 
     main = "Distribution de la Magnitude Stellaire", 
     xlab = "Magnitude (Plus petit = Plus brillant)", 
     col = "yellow", 
     breaks = 20)

boxplot(tab$luminosité, 
        main = "Boxplot Magnitude", 
        col = "yellow", 
        horizontal = TRUE)

#----------------------------------------------#
print("--- Résumé : Distance du système (Parsecs) ---")
summary(tab$distance)

hist(tab$distance, 
     main = "Distribution de la Distance du système", 
     xlab = "Distance (Parsecs)", 
     col = "grey", 
     breaks = 30)

boxplot(tab$distance, 
        main = "Boxplot Distance", 
        col = "grey", 
        horizontal = TRUE)