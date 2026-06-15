library(ape)
library(phytools)
library(ggtree)
library(geiger)

#import des données
Correl_max <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/Analyse sans biais/PhylANOVA_MAX_sans_biais.csv", header=T, sep = ";", row.names = "espece")

Correl_max <- Correl_max[-5,]
Correl_max <- Correl_max[-9,]

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
ymax <- as.numeric(unlist(Correl_max$FEX_max))

# Groupes
xmax <- as.factor(Correl_max$morphotype)

# Vérification
is.numeric(ymax)
is.factor(xmax)
print(ech_tree$tip.label)

phylANOVA(ech_tree, xmax, ymax, nsim=10000, posthoc=TRUE, p.adj="bonferroni")
