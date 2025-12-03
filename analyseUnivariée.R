
planetes = read.table("/home/cytech/Downloads/dataplanete.txt",header = TRUE,sep = ",",dec = ".",stringsAsFactors = FALSE)

names(planetes)          # noms des variables
dim(planetes)            # nb lignes / colonnes
summary(planetes)        # résumé numérique + factor
str(planetes)            # structure (type des variables)

# 1. Mise en forme des variables

# Variables qualitatives en factor
planetes$planet_type      = as.factor(planetes$planet_type)
planetes$detection_method = as.factor(planetes$detection_method)
planetes$year_class       = as.factor(planetes$year_class)

# (Optionnel) renommer les colonnes pour éviter les accents
names(planetes)[names(planetes) == "rayon_jup"]   <- "radius_jup"
names(planetes)[names(planetes) == "luminosité"]  <- "stellar_magnitude"

# Vérification
str(planetes)

# 2. Variables QUANTITATIVES (histogrammes + boxplots)

na = TRUE

### 2.1. Distribution de mass_jup 

summary(planetes$mass_jup)
mean(planetes$mass_jup, na.rm = na)
sd(planetes$mass_jup, na.rm = na)
quantile(planetes$mass_jup, na.rm = na)

hist(planetes$mass_jup,
     main = "Histogramme de mass_jup",
     xlab = "Masse (en masses de Jupiter)")

boxplot(planetes$mass_jup,
        main = "Boxplot de mass_jup",
        ylab = "mass_jup")

log_mass = log10(planetes$mass_jup[planetes$mass_jup > 0])

hist(log_mass,
     main = "Histogramme de mass_jup (log10 sécurisé)",
     xlab = "log10(mass_jup)",
     col = "lightblue")

boxplot(log_mass,
        main = "Boxplot de mass_jup (log10)",
        ylab = "log10(mass_jup)",
        col = "lightgrey")

### 2.2. Distribution de radius_jup

summary(planetes$radius_jup)
mean(planetes$radius_jup, na.rm = na)
sd(planetes$radius_jup, na.rm = na)
quantile(planetes$radius_jup, na.rm = na)

hist(planetes$radius_jup,
     main = "Histogramme de radius_jup",
     xlab = "Rayon (en rayons de Jupiter)")

radius_log = log10(planetes$radius_jup[planetes$radius_jup > 0])

hist(radius_log,
     main = "Histogramme de radius_jup (échelle log10)",
     xlab = "log10(radius_jup)",
     col = "lightblue",
     breaks = 20)
boxplot(planetes$radius_jup,
        main = "Boxplot de radius_jup",
        ylab = "radius_jup")

### 2.3. Distribution de orbital_radius 

summary(planetes$orbital_radius)
mean(planetes$orbital_radius, na.rm = na)
sd(planetes$orbital_radius, na.rm = na)
quantile(planetes$orbital_radius, na.rm = na)

hist(planetes$orbital_radius,
     main = "Histogramme de orbital_radius",
     xlab = "orbital_radius (UA)")

orbital_log = log10(planetes$orbital_radius[planetes$orbital_radius > 0])

hist(orbital_log,
     main = "Histogramme de orbital_radius (échelle log10)",
     xlab = "log10(orbital_radius)",
     col = "lightblue",
     breaks = 20)

boxplot(orbital_log,
        main = "Boxplot de orbital_radius",
        ylab = "orbital_radius")

### 2.4. Distribution de orbital_period 

summary(planetes$orbital_period)
mean(planetes$orbital_period, na.rm = na)
sd(planetes$orbital_period, na.rm = na)
quantile(planetes$orbital_period, na.rm = na)

hist(planetes$orbital_period,
     main = "Histogramme de orbital_period",
     xlab = "orbital_period (années / jours convertis)")

period_log = log10(planetes$orbital_period[planetes$orbital_period > 0])

hist(period_log,
     main = "Histogramme de orbital_period",
     xlab = "orbital_period")

boxplot(period_log,
        main = "Boxplot de orbital_period",
        ylab = "orbital_period")

### 2.5. Distribution de eccentricity 

summary(planetes$eccentricity)
mean(planetes$eccentricity, na.rm = na)
sd(planetes$eccentricity, na.rm = na)
quantile(planetes$eccentricity, na.rm = na)

hist(planetes$eccentricity,
     main = "Histogramme de eccentricity",
     xlab = "Excentricité")

boxplot(planetes$eccentricity,
        main = "Boxplot de eccentricity",
        ylab = "eccentricity")

### 2.6. Distribution de distance 

summary(planetes$distance)
mean(planetes$distance, na.rm = na)
sd(planetes$distance, na.rm = na)
quantile(planetes$distance, na.rm = na)

hist(planetes$distance,
     main = "Histogramme de distance",
     xlab = "Distance (parsecs)")

distance_log = log10(planetes$distance[planetes$distance > 0])

hist(distance_log,
     main = "Histogramme de distance",
     xlab = "Distance (parsecs)")

boxplot(planetes$distance,
        main = "Boxplot de distance",
        ylab = "distance")

### 2.7. Distribution de stellar_magnitude 

summary(planetes$stellar_magnitude)
mean(planetes$stellar_magnitude, na.rm = na)
sd(planetes$stellar_magnitude, na.rm = na)
quantile(planetes$stellar_magnitude, na.rm = na)

hist(planetes$stellar_magnitude,
     main = "Histogramme de stellar_magnitude",
     xlab = "Magnitude stellaire")

boxplot(planetes$stellar_magnitude,
        main = "Boxplot de stellar_magnitude",
        ylab = "stellar_magnitude")


# 3. Variables QUALITATIVES (tableaux + barplots)

### 3.1. Répartition des planet_type 

tab_type = table(planetes$planet_type)
tab_type                       # effectifs
prop.table(tab_type)           # fréquences

barplot(tab_type,
        main = "Répartition des types de planètes",
        xlab = "Type de planète",
        ylab = "Effectifs",
        las = 1)               

### 3.2. Répartition des detection_method 

tab_detect = table(planetes$detection_method)
tab_detect
prop.table(tab_detect)

barplot(tab_detect,
        main = "Méthodes de détection (échelle log sur effectifs)",
        ylab = "Effectifs (log)",
        xlab = "Méthode",
        las = 2,
        col = "skyblue",
        log = "y")      # log


### 3.3. Répartition des year_class 

tab_year = table(planetes$year_class)
tab_year
prop.table(tab_year)

barplot(tab_year,
        main = "Répartition des périodes de découverte (year_class)",
        xlab = "Période",
        ylab = "Effectifs",
        las = 2)

