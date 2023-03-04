##########1. Vorbereitung:##########
library(readxl)
library(stats)
library(tidyverse)
library(corrplot)
library(cluster)
library(ggplot2)
library(ggfortify)

#Einlesen des Datensatzes:
#Pfad ist anzupassen
playerdata <- read_excel(".../DF2122.xlsx", 
                         col_types = c("text", "text", "numeric", "text",
                                       "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric",
                                       "numeric", "numeric"))
View(playerdata) 

#Potenzielle Spalten für die Clusteranalyse auswählen:
datacluster <- playerdata[, c("Tkl", "TklG", "Blocks", "Int", "Cmp", "PrgDist",
                              "CmpLong", "xAG")]

#Auswahl von vier finalen Kennzahlen anhand der Zusammenhänge:
playerdata_S <- cor(datacluster, method = "spearman")
View(playerdata_S)
corrplot(playerdata_S, method = "number", number.cex = 0.75, tl.cex = 0.75,
         tl.col = "#929292")


##########2.1 Allgemeine Clusteranalyse:##########
datacluster_new <- playerdata[, c("TklG", "Int", "PrgDist", "xAG")]

#Daten normalisieren:
datacluster_norm <- scale(datacluster_new, center = TRUE, scale = TRUE)

#Optimale Clusteranzahl über WSS-Plot finden (Elbow-Kriterium):
wss <- (nrow(datacluster_norm)-1)*sum(apply(datacluster_norm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(datacluster_norm, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Anzahl der Cluster", ylab="WSS-Werte")

#Clusteranalyse durchführen: 
optimalclusters <- 5 #aufgrund des Elbow-Kriteriums
clusterresult <- kmeans(datacluster_norm, centers = optimalclusters, nstart=25)
autoplot(clusterresult, datacluster_norm, frame=TRUE)
print(clusterresult)

#Hauptkomponentenanalyse (PCA) durchführen:
pca_result <- prcomp(datacluster_norm)
#Beitrag der Variablen zu Hauptkomponenten PC1 / PC2:
pca_result$rotation[, 1:2]

#Clusterergebnisse in den Spielerdatensatz einfügen:
playerdata$cluster <- clusterresult$cluster
View(playerdata)

#Spieler im gleich Cluster wie Zielspieler finden:
find_player <- function(player_name) {
  player_row <- playerdata %>% filter(Player == player_name)
  if (nrow(player_row) == 0) {
    print(paste(player_name, "Spieler nicht im Datensatz gefunden."))
  } else {
    print(paste("Spieler", player_name, "gefunden in Cluster", player_row$cluster))
    clusterplayers <- playerdata %>% filter(cluster == player_row$cluster)
    print("Ähnliche Spieler:")
    print(clusterplayers$Player)
  }
}
#Beispielaufruf, hier variable Eingabe möglich:
find_player("Jacob Barrett Laursen")


##########2.2 Clusteranalyse zu den Defensivkennzahlen:##########
datacluster_new2 <- playerdata[, c("TklG", "Int", "Blocks")]

#Daten normalisieren:
datacluster_norm2 <- scale(datacluster_new2, center = TRUE, scale = TRUE)

#Optimale Clusteranzahl über WSS-Plot finden (Elbow-Kriterium):
wss <- (nrow(datacluster_norm2)-1)*sum(apply(datacluster_norm2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(datacluster_norm2, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Anzahl der Cluster", ylab="WSS-Werte")

#Clusteranalyse durchführen:
optimalclusters2 <- 4 #aufgrund des Elbow-Kriteriums
clusterresult2 <- kmeans(datacluster_norm2, centers = optimalclusters2, nstart=25)
autoplot(clusterresult2, datacluster_norm2, frame=TRUE)
print(clusterresult2)

#Hauptkomponentenanalyse (PCA) durchführen:
pca_result <- prcomp(datacluster_norm2)
#Beitrag der Variablen zu Hauptkomponenten PC1 / PC2:
pca_result$rotation[, 1:2]

#Clusterergebnisse in den Spielerdatensatz einfügen:
playerdata$cluster2 <- clusterresult2$cluster
View(playerdata)

#Spieler im gleich Cluster wie Zielspieler finden:
find_player2 <- function(player_name) {
  player_row <- playerdata %>% filter(Player == player_name)
  if (nrow(player_row) == 0) {
    print(paste(player_name, "Spieler nicht im Datensatz gefunden."))
  } else {
    print(paste("Spieler", player_name, "gefunden in Defensivcluster", player_row$cluster2))
    clusterplayers <- playerdata %>% filter(cluster2 == player_row$cluster2)
    print("Ähnliche Spieler im Defensivbereich:")
    print(clusterplayers$Player)
  }
}
#Beispielaufruf, hier variable Eingabe möglich:
find_player2("Benjamin Pavard")


##########2.3 Clusteranalyse zu den Offensivkennzahlen:##########
datacluster_new3 <- playerdata[, c("xAG", "PrgDist")]

#Daten normalisieren:
datacluster_norm3 <- scale(datacluster_new3, center = TRUE, scale = TRUE)

#Optimale Clusteranzahl über WSS-Plot finden (Elbow-Kriterium):
wss <- (nrow(datacluster_norm3)-1)*sum(apply(datacluster_norm3,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(datacluster_norm3, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Anzahl der Cluster", ylab="WSS-Werte")

#Clusteranalyse durchführen:
optimalclusters3 <- 3 #aufgrund des Elbow-Kriteriums
clusterresult3 <- kmeans(datacluster_norm3, centers = optimalclusters3, nstart=25)
autoplot(clusterresult3, datacluster_norm3, frame=TRUE)
print(clusterresult3)

#Hauptkomponentenanalyse (PCA) durchführen:
pca_result <- prcomp(datacluster_norm3)
#Beitrag der Variablen zu Hauptkomponenten PC1 / PC2:
pca_result$rotation[, 1:2]

#Clusterergebnisse in den Spielerdatensatz einfügen:
playerdata$cluster3 <- clusterresult3$cluster
View(playerdata)

#Spieler im gleich Cluster wie Zielspieler finden:
find_player3 <- function(player_name) {
  player_row <- playerdata %>% filter(Player == player_name)
  if (nrow(player_row) == 0) {
    print(paste(player_name, "Spieler nicht im Datensatz gefunden."))
  } else {
    print(paste("Spieler", player_name, "gefunden in Offensivcluster", player_row$cluster3))
    clusterplayers <- playerdata %>% filter(cluster3 == player_row$cluster3)
    print("Ähnliche Spieler im Offensivbereich:")
    print(clusterplayers$Player)
  }
}
#Beispielaufruf, hier variable Eingabe möglich:
find_player3("Mats Hummels")
