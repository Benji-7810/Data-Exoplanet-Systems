
tab = read.table("data_planetes.csv", 
                 header = TRUE, 
                 sep = ",", 
                 stringsAsFactors = FALSE 
)
d
tab

summary(tab)

## mathode APC
tab_quanti <- subset(tab, select = -c(name, planet_type, detection_method, year_class, discovery_year, eccentricity))

# Maintenant le filtre et le log vont marcher
tab_quanti <- tab_quanti[rowSums(tab_quanti <= 0) == 0, ]
tab_quanti <- log(tab_quanti)

library(FactoMineR)
library(factoextra)

res.tab_quanti = PCA(tab_quanti, graph = FALSE)
fviz_eig(res.tab_quanti, addlabels = TRUE)
fviz_pca_var(res.tab_quanti, col.var = "black")
tab_quanti
### methode AFC

### type de planet et méthode de detection
tab <- read.csv("data_planetes.csv", header=TRUE, stringsAsFactors=FALSE)


data <- table(tab$planet_type, tab$detection_method)


data <- data[, colSums(data) > 10]
data <- as.matrix(data) 


library(FactoMineR)
library(factoextra)

res.ca = CA(data) # Commande AFC
res.ca$eig
res.ca$row$cos2


### years_class et methode detection

data2 <- table(tab$year_class, tab$detection_method)


data2 <- as.matrix(data2) 
data2

library(FactoMineR)
library(factoextra)

res.ca = CA(data2) # Commande AFC
res.ca$eig
res.ca$row$cos2

### years_class et methode detection

data3 <- table(tab$year_class, tab$planet_type)d


data3 <- as.matrix(data3) 
data3


library(FactoMineR)
library(factoextra)

res.ca = CA(data3) # Commande AFC
res.ca$eig
res.ca$row$cos2



