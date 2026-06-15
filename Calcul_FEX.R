library(seewave)
library(tuneR)
library(dplR)
library(tools)
library(Rraven)
library(warbleR)
library(Rraven)
library(SoundShape)
library(wav)
library(stringr)
library(ape)
library(geomorph)
library(pals)
library(vegan)
library(ggplot2)

#### DOSSIERS #################################
#dossier avec les enregistrements bruts
raw <- c("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/Données/enregistrements")

#dossier avec les tables de sélection qui ont le même nom que les enregistrements
sel.table <- c("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/Données/tableaux")
###############################################


#### IMPORTATION DONNEES ######################
#importation tables de selection (Rraven)
selection_tables_bind_2 <- imp_raven(sound.file.col="End.file", freq.cols=TRUE, path = sel.table, warbler.format = TRUE)
View(selection_tables_bind_2)

#importation enregistrements (tuneR)
cut_2 <- import.cut(sel.table = selection_tables_bind_2, dossier = "/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/Données/enregistrements")
cut_2
cut_2[[1]]
length(cut_2)

for (i in 1:length(selection_tables_bind_2$sound.files)) {
  selection_tables_bind_2$cut.name[i] <- names(cut_2)[i]
}
###############################################



#### BAND PASS FILTER #########################
#filtration (seewave)
filtered_cut_2 <- filtration(cut.recordings = cut_2, dossier = c("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/Données/enregistrements_filtered"), sel.table = selection_tables_bind_2)
filtered_cut_2
length(filtered_cut_2)
###############################################



#### CALCUL FEX ################################
#fex (podos 2016, Nyniane)
fex_calcul_256 <- fex.r(data = filtered_cut_2, sel.table = selection_tables_bind_2)
###############################################


#### PLOT #####################################
#mise en forme des noms de la liste
#importer tableau avec la correspondance abbréviation/nom d'espece
correspondance <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/Données/Names.csv", sep=";", header=TRUE)

#tableau avec le nom de l'espece pour chaque individu acoustique
m <- matrix(data='', ncol = 2, nrow = length(fex_calcul_256))
factors <- data.frame(m)
colnames(factors) <- c('Individus', 'Espece')
factors
for (i in 1:length(fex_calcul_256)) {
  factors$Individus[i] <- names(fex_calcul_256)[i]
  species <- str_extract(factors$Individus[i], "[^_]+")
  factors$Espece[i] <- correspondance$NComp[correspondance$NAbr == species]
}
factors

#remplacer les noms de fex par les noms d'especes
names(fex_calcul_256) <- factors$Espece
fex_calcul_256

#names(fex_calcul_256)<-as.factor(names(fex_calcul_256))
#data.fex <- do.call(rbind.data.frame)
#data.fex <- tibble::as_tibble(as.list(fex_calcul_256))


###############################################


# plot 
new_order <- with(stack(fex_calcul_256), reorder(ind , values, median , na.rm=T))
boxplot(values ~ new_order, data = stack(fex_calcul_256))

ggplot(stack(fex_calcul_256), aes(new_order, values)) + 
  geom_boxplot(aes(fill = new_order)) + 
  labs(title = "FEX", x = "Species", y = "FEX") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Affichage des MOY et MED############

dat <- stack(fex_calcul_256)

new_order <- with(dat, reorder(ind, values, mean, na.rm = TRUE))

ggplot(dat, aes(new_order, values)) + 
  geom_boxplot(aes(fill = new_order)) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 2) +
  stat_summary(fun = mean, geom = "text",
               aes(label = round(..y.., 2)),
               vjust = -0.5, color = "black", size = 3) +
  labs(title = "FEX", x = "Species", y = "FEX") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat <- stack(fex_calcul_256)

new_order <- with(dat, reorder(ind, values, median, na.rm = TRUE))

ggplot(dat, aes(new_order, values)) + 
  geom_boxplot(aes(fill = new_order)) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 2) +
  stat_summary(fun = median, geom = "text",
               aes(label = round(..y.., 2)),
               vjust = -0.5, color = "black", size = 3) +
  labs(title = "FEX", x = "Species", y = "FEX") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FEX Total############  


FEXTotal <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Acoustique/TotalFEX.csv", sep=";", header=TRUE)    


dat2 <- data.frame(Especes = FEXTotal$Species, Indice_FEX = FEXTotal$Values)

ggplot(dat2, aes(reorder(Especes, Indice_FEX, mean), Indice_FEX)) + 
  geom_boxplot(aes(fill = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affichage des MOY############  

dat2 <- data.frame(Especes = FEXTotal$Species, Indice_FEX = FEXTotal$Values)

ggplot(dat2, aes(reorder(Especes, Indice_FEX), Indice_FEX)) + 
  geom_boxplot(aes(fill = NULL)) + 
  stat_summary(fun = mean, geom = "text",
               aes(label = round(..y.., 2)),
               vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affichage des MED############ 

ggplot(dat2, aes(reorder(Especes, Indice_FEX, median), Indice_FEX)) + 
  geom_boxplot(aes(fill = NULL)) + 
  stat_summary(fun = median, geom = "text",
               aes(label = round(..y.., 2)),
               vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
