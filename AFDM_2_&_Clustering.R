#####AFDM & Clustering du Sous-groupe#####

library(FactoMineR)
library(factoextra)

#import des données
hyoidNUM <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Anatomie/Analyses R/Clusters FAMD/Rajout AVLE/HyoidC2Test.csv", header=T, sep = ";", row.names="A")
hyoidXClusters <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Anatomie/Analyses R/Clusters FAMD/Rajout AVLE/HyoidC2Test.csv", header=T, sep = ";", row.names="A")

#Passage des caractères qualitatifs en facteurs
hyoidXClusters$Pgl.Soudé <- as.factor(hyoidXClusters$Pgl.Soudé)
hyoidXClusters$Fte.pgl <- as.factor(hyoidXClusters$Fte.pgl)
hyoidXClusters$Fme.Pgl <- as.factor(hyoidXClusters$Fme.Pgl)
hyoidXClusters$Crb.Pgl <- as.factor(hyoidXClusters$Crb.Pgl)
hyoidXClusters$Pgl.Cornes <- as.factor(hyoidXClusters$Pgl.Cornes)
hyoidXClusters$Fme.Bhl <- as.factor(hyoidXClusters$Fme.Bhl)
hyoidXClusters$Uhl <- as.factor(hyoidXClusters$Uhl)
hyoidXClusters$Uhl.Plat <- as.factor(hyoidXClusters$Uhl.Plat)
hyoidXClusters$Uhl.jointif.Bhl <- as.factor(hyoidXClusters$Uhl.jointif.Bhl)

#lancement de la procédure
hyoidXClusters$Uhl <- NULL
hyoidNUM$Uhl <- NULL
afdm.hyoid <- FAMD(hyoidXClusters,ncp=5)

#affichage des résultats
print(summary(afdm.hyoid))

# Contribution to the first dimension
fviz_contrib(afdm.hyoid, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(afdm.hyoid, "var", axes = 2)



#Clustering

# Standardize the data
df <- scale(hyoidNUM)

# Show the first 6 rows
head(df, nrow = 6)

# Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(hyoidNUM, method = "euclidean")

as.matrix(res.dist)[1:26, 1:26]

res.hc <- hclust(d = res.dist, method = "ward.D2")

# cex: label size
library("factoextra")
fviz_dend(res.hc, cex = 0.5)

# Cut tree into 5 groups
grp <- cutree(res.hc, k = 5)
head(grp, n = 5)

fviz_dend(res.hc, k = 5, 
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#672"))

hc.cut <- hcut(res.dist, k = 5, hc_method = "complete")

fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)

fviz_cluster(hc.cut, hyoidNUM, ellipse.type = "convex")
