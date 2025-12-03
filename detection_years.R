# 1. Chargement et Nettoyage
tab <- read.csv("data_planetes.csv", header=TRUE, stringsAsFactors=FALSE)


# Boxplot simple
boxplot(discovery_year ~ detection_method,
        data = tab,
        las = 2,              # rotation des labels
        col = "lightblue",    # couleur simple
        border = "black",
        main = "Année de découverte selon la méthode",
        xlab = " ",
        ylab = "Année de découverte")


# --- 2. Numérique : ANOVA sur le Log ---
# On cherche à expliquer le LOG de la masse par le type
fit_log <- lm(tab$discovery_year ~ as.factor(tab$detection_method))
anova(fit_log)

eta = 78257/(78257+6969)
eta


print(paste("Rapport de corrélation (Eta^2) :", round(eta, 4)))

