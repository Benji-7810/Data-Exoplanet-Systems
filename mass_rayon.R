
tab <- read.csv("data_planetes.csv", header=TRUE, stringsAsFactors=FALSE)

#----------------------quanti quanti--------------------------
#-------------------------------------------------------------

# On enlève les valeurs nulles ou négatives pour le log
tab_clean <- subset(tab, mass_jup > 0 & rayon_jup > 0)

# 2. Tracer le nuage de points (Données brutes)
plot(tab_clean$mass_jup, tab_clean$rayon_jup,
     main = "Relation Masse et Rayon (Brut)",
     xlab = "Masse (Jupiters)", 
     ylab = "Rayon (Jupiters)")
# Constat : On voit une courbe, pas une droite. Le modèle linéaire simple ne marchera pas.

# 3. Calculer la corrélation linéaire brute
cor(tab_clean$mass_jup, tab_clean$rayon_jup) 
# 0.32 éclaté

# --- PASSAGE AUX LOGARITHMES (Comme Exo 4, étape 6) ---

# 4. Transformation des variables
u = log(tab_clean$mass_jup)   
v = log(tab_clean$rayon_jup)  

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


# Prévision pour une masse de 1 Jupiter (X = 1)
u1 = log(1) # log(1) vaut 0

# Calcul de v1 (log du rayon prévu)
# v1 = pente * u1 + ordonnée_à_l_origine
v1 = mod2$coefficients[2] * u1 + mod2$coefficients[1]

# Retour à la variable d'origine (exponentielle)
y1 = exp(v1)

y1 
#  0.8577 > 80 le modele est validé


#ccl 
#Nous observons une corrélation positive : les planètes plus massives sont plus volumineuses.
#Cependant, la pente faible (0.36) indique un effet de compression. 
#Une planète gazeuse très massive ne devient pas énormément plus grande, 
#elle devient surtout plus dense car sa propre gravité la compresse sur elle-même.
#Nous observons une corrélation positive : les planètes plus massives sont plus volumineuses. 
#, la pente faible (0.36) indique un effet de compression. Une planète gazeuse très massive 
#ne devient pas énormément plus grande, elle devient surtout plus dense car sa propre gravité 
#la compresse sur elle-même.

