library(corrplot)
library(ggplot2)
library(dplyr)
library(nortest)
library(correlation)
library(readxl)

#Einlesen des Datensatzes
#Pfad ist anzupassen
BL1722 <- read_excel(".../Mannschaftsdaten.xlsx", 
                     col_types = c("numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))
View(BL1722)

#1.Tests im Vorfeld:
#Funktion zur Visualisierung des Zusammenhangs der Variablen zu Pts:
plot_relation <- function(df, y) {
  y_name <- deparse(substitute(y))
  ggplot(df, aes(x = Pts, y = y)) +
    geom_point(size = 2, col = "#f88379") +
    labs(x = "BL1722$Pts", y = y_name) +
    ggtitle(paste("Zusammenhang von BL1722$Pts und", y_name)) +
    theme(plot.title = element_text(hjust = 0.4))
}

#Durchführung der Visualisierung für alle Variablen:
#Linearer Zusammenhang erkannbar:
plot_relation(BL1722, BL1722$Poss)
plot_relation(BL1722, BL1722$Sh)
plot_relation(BL1722, BL1722$SoT)
plot_relation(BL1722, BL1722$Cmp)
plot_relation(BL1722, BL1722$`Cmp%`)
plot_relation(BL1722, BL1722$Prog)
plot_relation(BL1722, BL1722$CK)
plot_relation(BL1722, BL1722$xG)
plot_relation(BL1722, BL1722$xAG)
plot_relation(BL1722, BL1722$xGA)
#Linearer Zusammenhang mit starken Ausreißern:
plot_relation(BL1722, BL1722$KP)
#Kein Zusammenhang erkennbar:
plot_relation(BL1722, BL1722$Succ)
plot_relation(BL1722, BL1722$`Tkl%`)
plot_relation(BL1722, BL1722$`Save%`)

#Test der genutzten Korrelationsmethode mit Auto-Correlation:
correlation(BL1722, include_factors = TRUE, method = "auto")

#2. Eigentliche Korrelationsanalyse:
#2.1 Pearson:
#Korrelationsmatrix und Matrix mit p-Wert erstellen:
BL1722_P <- cor(BL1722, method = "pearson")
BL1722_Pp <- cor.mtest(BL1722_P)
View(BL1722_P)
corrplot(BL1722_P, method = "number", p.mat = BL1722_Pp$p,
         number.cex = 0.55, tl.cex = 0.75, tl.col = "#929292")

#2.2 Spearman:
#Korrelationsmatrix und Matrix mit p-Wert erstellen:
BL1722_S <- cor(BL1722, method = "spearman")
BL1722_Sp <- cor.mtest(BL1722_S)
View(BL1722_S)
corrplot(BL1722_S, method = "number", p.mat = BL1722_Sp$p,
         number.cex = 0.55, tl.cex = 0.75, tl.col = "#929292")

#3. Vergleich der Ergebnisse:
difference <- abs(BL1722_P -BL1722_S)
View(difference)
max_difference <- max(difference)
print(max_difference)
min_difference <- min(difference)
print(min_difference)
mean_difference <- mean(difference)
print(mean_difference)




