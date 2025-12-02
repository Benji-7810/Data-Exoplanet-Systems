# 1. Chargement et Nettoyage
tab <- read.csv("data_planetes.csv", header=TRUE, stringsAsFactors=FALSE)
tab <- subset(tab, mass_jup > 0) # Sécurité pour le log


tab$log_mass <- log(tab$mass_jup)

# --- 1. GRAPHIQUE AMÉLIORÉ ---
boxplot(tab$log_mass ~ tab$planet_type, 
        main="Distribution de la Masse (Log) par Type",
        col=c("gold", "royalblue", "brown", "grey"), # Une couleur par boîte
        las=2, # Écriture verticale des étiquettes
        ylab="Log(Masse Jupiter)",
        xlab="") # On enlève le label X car les noms sont déjà là



# --- 2. Numérique : ANOVA sur le Log ---
# On cherche à expliquer le LOG de la masse par le type
fit_log <- lm(tab$log_mass ~ as.factor(tab$planet_type))
anova(fit_log)

eta = 22806.3/(22806.3+4202.7)
eta


print(paste("Rapport de corrélation (Eta^2) :", round(eta, 4)))

#Le rapport de corrélation $\eta^2$ est de 0.84.
#Cela signifie que 84% de la variabilité de la masse (en log) est expliquée uniquement
#par le type de planète. C'est un lien très fort : 
#connaître le type de la planète permet de deviner sa masse avec une grande précision.