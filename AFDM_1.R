#####AFDM avec FactoMineR######

library(readxl)
library(FactoMineR)
library(factoextra)

#Groupe Total

#import des données
hyoidFCTOR <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Anatomie/Analyses R/HyoidC-NvxCrc.csv", header=T, sep = ";", row.names="A")

#Passage des caractères qualitatifs en facteurs
hyoidFCTOR$Pgl.Soudé <- as.factor(hyoidFCTOR$Pgl.Soudé)
hyoidFCTOR$Fte.pgl <- as.factor(hyoidFCTOR$Fte.pgl)
hyoidFCTOR$Fme.Pgl <- as.factor(hyoidFCTOR$Fme.Pgl)
hyoidFCTOR$Crb.Pgl <- as.factor(hyoidFCTOR$Crb.Pgl)
hyoidFCTOR$Pgl.Cornes <- as.factor(hyoidFCTOR$Pgl.Cornes)
hyoidFCTOR$Fme.Bhl <- as.factor(hyoidFCTOR$Fme.Bhl)
hyoidFCTOR$Uhl <- as.factor(hyoidFCTOR$Uhl)
hyoidFCTOR$Uhl.Plat <- as.factor(hyoidFCTOR$Uhl.Plat)
hyoidFCTOR$Uhl.jointif.Bhl <- as.factor(hyoidFCTOR$Uhl.jointif.Bhl)

#lancement de la procédure
afdm.hyoid2 <- FAMD(hyoidFCTOR,ncp=5)
#affichage des résultats
print(summary(afdm.hyoid2))

# Contribution to the first dimension
fviz_contrib(afdm.hyoid2, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(afdm.hyoid2, "var", axes = 2)

