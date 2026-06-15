library(ape)
library(phytools)
library(ggtree)
library(geiger)

#import des données
Correl_max_23 <- read.csv("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/Analyse sans biais/PhylANOVA_MAX_sans_biais.csv", header=T, sep = ";", row.names = "espece")

Correl_max_23$morphotype <- as.factor(Correl_max_23$morphotype)

ggplot(Correl_max_23, aes(x = morphotype, y = FEX_max)) +
  geom_boxplot(fill = "lightgrey") +
  geom_jitter(aes(color = morphotype), width = 0.2, alpha = 0.6) +
  theme(legend.position = "none")

#Construction de l'arbre

tree <- read.nexus("/Users/flavienterreaux/Documents/MNHN - Master SEP/M1/Stage/Stage MNHN/Partie Combinaison des 2/tree total aves 1.4.nex")
ech23 <- c("Crypturellus_soui","Coturnix_coturnix","Clangula_hyemalis","Tachybaptus_dominicus","Tauraco_persa","Mesitornis_unicolor","Burhinus_senegalensis","Caprimulgus_europaeus","Steatornis_caripensis","Eurypyga_helias","Cochlearius_cochlearius","Lipaugus_vociferans","Elaenia_flavogaster","Furnarius_rufus","Parus_major","Psittacula_krameri","Falco_tinnunculus","Ceryle_rudis","Picus_viridis","Colius_striatus","Tyto_alba","Otus_scops","Aviceda_leuphotes") #list avec les especes de ton échantillon
ech_tree_23 <- ape::drop.tip(tree, tree$tip.label[-match(ech23, tree$tip.label)])

# Arbre lisible
plot(
  ech_tree_23,
  type = "phylogram",     # orientation arbre
  cex = 0.8,              # taille du texte
  font = 3,               # italique
  no.margin = TRUE
)


#Phylanova

# Variable
y_max <- as.numeric(unlist(Correl_max_23$FEX_max))

# Groupes
x_max <- as.factor(Correl_max_23$morphotype)

# Vérification
is.numeric(y_max)
is.factor(x_max)
print(ech_tree_23$tip.label)

phylANOVA(ech_tree_23, x_max, y_max, nsim=10000, posthoc=TRUE, p.adj="bonferroni")


