
planetes = read.table("/home/cytech/Downloads/dataplanete.txt",header = TRUE,sep = ",",dec = ".",stringsAsFactors = FALSE)


# Transformer detection_method en factor
planetes$detection_method = as.factor(planetes$detection_method)

# On garde les lignes où mass_jup n'est pas manquante
df = subset(planetes, !is.na(mass_jup) & !is.na(detection_method))

# Moyenne de mass_jup par méthode
tapply(df$mass_jup, df$detection_method, mean, na.rm = TRUE)

# Médiane
tapply(df$mass_jup, df$detection_method, median, na.rm = TRUE)

# Ecart-type
tapply(df$mass_jup, df$detection_method, sd, na.rm = TRUE)

boxplot(mass_jup ~ detection_method,
        data = df,
        main = "Mass_jup selon méthode de détection",
        ylab = "mass_jup (M_Jup)",
        xlab = "Méthode",
        las = 2,
        col = "lightblue")

df$log_mass = log10(df$mass_jup[df$mass_jup > 0])

boxplot(log_mass ~ detection_method,
        data = df,
        main = "Mass_jup (log10) selon méthode de détection",
        xlab = "Méthode",
        ylab = "log10(mass_jup)",
        las = 2,
        col = "lightgreen")

library(vioplot)

methods = levels(df$detection_method)

vioplot_list = lapply(methods, function(m){
  log10(df$mass_jup[df$detection_method == m & df$mass_jup > 0])
})

vioplot(vioplot_list,
        names = methods,
        col = "skyblue",
        main = "Distribution log10(mass_jup) par méthode")

##Les planètes les plus massives sont principalement détectées 
#par la méthode Radial Velocity, ce qui est cohérent avec la physique : 
#plus une planète est massive, plus elle produit une oscillation mesurable sur son étoile.