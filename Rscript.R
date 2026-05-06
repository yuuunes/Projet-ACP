# =========================
# 1. Chargement des packages
# =========================
library(FactoMineR)
library(factoextra)
library(dplyr)

# =========================
# 2. Import des données
# =========================
data <- read.csv("freMTPL2freq.csv")
data_reduit <- data[1:100, ]
head(data_reduit)

# Manipulation des données 

rownames(data_reduit) <- data_reduit$IDpol
data_reduit$IDpol <- NULL
View(data_reduit)

# =========================
# 5. ACP (centrée-réduite)
# =========================

res.pca <- PCA(data_reduit,
               scale.unit = TRUE,   # standardisation
               ncp = 5,            # nombre de dimensions
               quali.sup = c(7, 8, 9, 11), # variables qualitatives ou supplémaentaires
               graph = TRUE)

# fviz_pca_var(res.pca)

summary(res.pca)

# Valeurs propres
eig.val <- res.pca$eig
print(eig.val)

# =========================
# 7. Visualisations

# Scree plot (variance expliquée)
fviz_eig(res.pca, addlabels = TRUE)


# Visualisation avec variables qualitatives
fviz_pca_biplot(res.pca,
                repel = TRUE)

# =========================
# 8. Contribution des variables

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


# =========================
# 10. Visualisation des modalités qualitatives

fviz_pca_ind(res.pca,
             habillage = "Region")
