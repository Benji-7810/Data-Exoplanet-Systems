
planetes = read.table("/home/cytech/Downloads/dataplanete.txt",header = TRUE,sep = ",",dec = ".",stringsAsFactors = FALSE)

names(planetes)
str(planetes)
summary(planetes)


# 1. PRÉPARATION DES VARIABLES (X = orbital_radius, Y = mass_jup)

X = planetes$orbital_radius
Y = planetes$mass_jup

# On enlève les NA et les valeurs <= 0 (pour les logs après)
ind = which(!is.na(X) & !is.na(Y) & X > 0 & Y > 0)
X = X[ind]
Y = Y[ind]

# 2. NUAGES DE POINTS (SCATTER PLOTS)


## 2.1. Nuage linéaire simple

plot(X, Y,
     main = "mass_jup en fonction de orbital_radius",
     xlab = "orbital_radius (UA)",
     ylab = "mass_jup (M_Jup)",
     pch  = 16)

## 2.2. Nuage en échelle log-log 

plot(log10(X), log10(Y),
     main = "mass_jup vs orbital_radius (log10-log10)",
     xlab = "log10(orbital_radius)",
     ylab = "log10(mass_jup)",
     pch  = 16)

# directement avec axes log) :
plot(X, Y,
     main = "mass_jup vs orbital_radius (axes log)",
     xlab = "orbital_radius (UA)",
     ylab = "mass_jup (M_Jup)",
     pch  = 16,
     log  = "xy")


# 3. COVARIANCE ET CORRÉLATION (COMME LE COURS)


## 3.1. Covariance et corrélation en échelle linéaire 

cov_lin  = cov(X, Y)
cor_lin  = cor(X, Y)

cov_lin
cor_lin

## 3.2. Covariance et corrélation en log10-log10 

logX = log10(X)
logY = log10(Y)

cov_log = cov(logX, logY)
cor_log = cor(logX, logY)

cov_log
cor_log


# 4. RÉGRESSION LINÉAIRE (SUR LES LOGS)
#    log10(mass_jup) ~ log10(orbital_radius)
#    (Même logique que: mod <- lm(temps ~ Notes))

mod = lm(logY ~ logX)
summary(mod)          # coefficients, R^2, etc.

# Nuage + droite de régression
plot(logX, logY,
     main = "Régression linéaire log-log : mass_jup ~ orbital_radius",
     xlab = "log10(orbital_radius)",
     ylab = "log10(mass_jup)",
     pch  = 16)

abline(mod, col = "red", lwd = 2)


# 5. LES GÉANTES SONT-ELLES PLUS PROCHES
#    OU PLUS LOIN DE LEUR ÉTOILE ?

# (et on définit les géantes : mass_jup >= 1 M_Jup par exemple)
planetes$giant = NA
planetes$giant[planetes$mass_jup >= 1]  = "Geante"
planetes$giant[planetes$mass_jup <  1]  = "Non_geante"
planetes$giant = as.factor(planetes$giant)

# Distances des planètes géantes / non-géantes
tapply(planetes$orbital_radius,
       planetes$giant,
       mean,
       na.rm = TRUE)

tapply(planetes$orbital_radius,
       planetes$giant,
       median,
       na.rm = TRUE)

# Boxplot comparatif des distances
boxplot(orbital_radius ~ giant,
        data = planetes,
        main = "Orbital radius selon le type (géante / non-géante)",
        xlab = "Type de planète",
        ylab = "orbital_radius (UA)",
        log  = "y")   # log sur l'axe Y si beaucoup de dispersion

