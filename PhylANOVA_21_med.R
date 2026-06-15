library(ape)
library(phytools)
library(ggtree)
library(geiger)

#import des données
Correl_med <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/Analyse sans biais/PhylANOVA_MED_sans_biais.csv", header=T, sep = ";", row.names = "espece")

Correl_med <- Correl_med[-5,]
Correl_med <- Correl_med[-9,]

#Construction de l'arbre

tree <- read.nexus("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/tree total aves 1.4.nex")
ech <- c("Crypturellus_soui","Coturnix_coturnix","Clangula_hyemalis","Tachybaptus_dominicus","Tauraco_persa","Mesitornis_unicolor","Burhinus_senegalensis","Caprimulgus_europaeus","Steatornis_caripensis","Eurypyga_helias","Cochlearius_cochlearius","Lipaugus_vociferans","Elaenia_flavogaster","Furnarius_rufus","Parus_major","Falco_tinnunculus","Ceryle_rudis","Colius_striatus","Tyto_alba","Otus_scops","Aviceda_leuphotes") #list avec les especes de ton échantillon
ech_tree <- ape::drop.tip(tree, tree$tip.label[-match(ech, tree$tip.label)])

# Arbre lisible
plot(
  ech_tree,
  type = "phylogram",     # orientation arbre
  cex = 0.8,              # taille du texte
  font = 3,               # italique
  no.margin = TRUE
)


#Phylanova

# Variable
ymed <- as.numeric(unlist(Correl_med$FEX_med))

# Groupes
xmed <- as.factor(Correl_med$morphotype)

# Vérification
is.numeric(ymed)
is.factor(xmed)
print(ech_tree$tip.label)

phylANOVA(ech_tree, xmed, ymed, nsim=10000, posthoc=TRUE, p.adj="bonferroni")
