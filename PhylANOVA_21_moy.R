library(ape)
library(phytools)
library(ggtree)
library(geiger)

#import des données
Correl_moy <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/Analyse sans biais/PhylANOVA_MOY_sans_biais.csv", header=T, sep = ";", row.names = "espece")

Correl_moy <- Correl_moy[-5,]
Correl_moy <- Correl_moy[-9,]

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
y <- as.numeric(unlist(Correl_moy$FEX_mean))

# Groupes
x <- as.factor(Correl_moy$morphotype)

# Vérif
is.numeric(y)
is.factor(x)
print(ech_tree$tip.label)

phylANOVA(ech_tree, x, y, nsim=10000, posthoc=TRUE, p.adj="bonferroni")

