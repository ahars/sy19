# UV : SY19 - TP01
# 3. Les données de distances entre aéroports
# Auteurs : Alice Ngwembou - Antoine Hars
# Fichier : exercice3.R

library(MASS)

#setwd("Z:/")
airports <- read.table("airports2.txt", header = F, row.names = 1)

# Matrice des données et tableau des distances.
airmat = as.matrix(airports)
airdist = as.dist(airports)

# Question 1)
# Réalisation de l'AFTD sur les données
aftd = cmdscale(airdist)
png(file = "plots/plot_airports_cmdscale.png")
plot(aftd, type = "n", main = "Représentation de airports au moyen de l'AFTD", xlab = "axe1", ylab = "axe2")
text(aftd, labels(airdist))
dev.off()

### Question 2)

# Projection de Sammon
sammonmat = sammon(airdist)
png(file = "plots/plot_airports_sammon.png")
plot(sammonmat$points, type = "n", main = "Représentation de airports par Sammon", xlab = "axe1", ylab = "axe2")
text(sammonmat$points, labels(airdist))
dev.off()

# Projection de Kruskal
kruskalmat = isoMDS(airdist)
png(file = "plots/plot_airports_kruskal.png")
plot(kruskalmat$points, type = "n", main = "Représentation de airports par Kruskal", xlab = "axe1", ylab = "axe2")
text(kruskalmat$points, labels(airdist))
dev.off()

# Représentation de Shepard
png(file = "plots/plot_airports_shepard_cmdscale.png")
s1 = Shepard(airdist, aftd)
plot(s1, main = "diagramme de Shepard de airports (cmdscale)", pch = "*")
abline(0, 1)
dev.off();

s2 = Shepard(airdist, sammonmat$points)
png(file = "plots/plot_airports_shepard_sammon.png")
plot(s2, main = "diagramme de Shepard de airports (sammon)", pch = "*")
abline(0, 1)
dev.off()

s3 = Shepard(airdist, kruskalmat$points)
png(file = "plots/plot_airports_shepard_kruskal.png")
plot(s3, main = "diagramme de Shepard de airports (kruskal)", pch = "*")
abline(0, 1)
dev.off()

### Question 3)

# Suppression des villes non européennes
euromat = airmat[-1:-3,-1:-3]
euromat = euromat[-4:-6,-4:-6]
euromat = euromat[-2,-2]
euromat = euromat[-5:-6,-5:-6]
euromat = euromat[-6,-6] # Suppression Los Angeles
euromat = euromat[-7,-7] # Suppression Montreal
euromat = euromat[-7:-8,-7:-8] # Suppression Moscou & New-Yorj
euromat = euromat[-8:-9,-8:-9] # Suppression Pekin & Pretoria
euromat = euromat[-9:-10,-9:-10] # Suppression SF & Sydney
euromat = euromat[-9,-9] # Suppression Tokyo
euromat = euromat[-10,-10] # Suppression Wellington

# Transformation en matrice de distances
eurodist = as.dist(euromat)

# AFTD
euroaftd = cmdscale(eurodist, 5)
png(file = "plots/plot_euro_cmdscale.png")
plot(euroaftd, type = "n", main = "Représentation de airports européens par cmdscale", xlab = "axe1", ylab = "axe2")
text(euroaftd, labels(eurodist))
dev.off()

# Comme on cherche à représenter les points sur un plan et non une sphère ...
sammoneuromat = sammon(eurodist)
png(file = "plots/plot_euro_sammon.png")
plot(sammoneuromat$points, type = "n", main = "Représentation de airports européens par Sammon", xlab = "axe1", ylab = "axe2")
text(sammoneuromat$points, labels(eurodist))
dev.off()

# Projection de Kruskal
kruskaleuromat = isoMDS(eurodist)
png(file = "plots/plot_euro_kruskal.png")
plot(kruskaleuromat$points, type = "n", main = "Représentation de airports européens par Kruskal", xlab = "axe1", ylab = "axe2")
text(kruskaleuromat$points, labels(eurodist))
dev.off()

# Représentation de Shepard
png(file = "plots/plot_euro_shepard_cmdscale.png")
s4 = Shepard(eurodist, euroaftd)
plot(s4, main = "diagramme de Shepard de airports europe (cmdscale)", pch = "*")
abline(0, 1)
dev.off();

s5 = Shepard(eurodist, sammoneuromat$points)
png(file = "plots/plot_euro_shepard_sammon.png")
plot(s5, main = "diagramme de Shepard de airports europe (sammon)", pch = "*")
abline(0, 1)
dev.off()

s6 = Shepard(eurodist, kruskaleuromat$points)
png(file = "plots/plot_euro_shepard_kruskal.png")
plot(s6, main = "diagramme de Shepard de airports europe (kruskal)", pch = "*")
abline(0, 1)
dev.off()

