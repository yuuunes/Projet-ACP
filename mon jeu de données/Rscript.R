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

# Manipulation des données 

rownames(data_reduit) <- data_reduit$IDpol
data_reduit$IDpol <- NULL
View(data_reduit)
# =========================
# 3. Séparation variables
# =========================

# Variables quantitatives (pour ACP)
# data_quant <- data_reduit %>%
#   select(ClaimNb, Exposure, VehPower, VehAge, DrivAge, BonusMalus, Density)
# 
# # Variables qualitatives (pour projection après)

# data_quali <- data_reduit %>%
#   select(VehBrand, VehGas, Area, Region)

# =========================
# 5. ACP (centrée-réduite)
# =========================

res.pca <- PCA(data_reduit,
               scale.unit = TRUE,   # standardisation
               ncp = 5,            # nombre de dimensions
               quali.sup = c(7, 8, 9, 11), # variables qualitatives ou supplémaentaires
               graph = TRUE)

summary(res.pca)
# =========================
# 6. Résultats de base
# =========================

# Valeurs propres
eig.val <- res.pca$eig
print(eig.val)

# =========================
# 7. Visualisations
# =========================

# Scree plot (variance expliquée)
fviz_eig(res.pca, addlabels = TRUE)

# Visualisation avec variables qualitatives
fviz_pca_biplot(res.pca,
                repel = TRUE)

# Cercle des corrélations
fviz_pca_var(res.pca,
             col.var = "contrib",
             gradient.cols = c("blue", "orange", "red"),
             repel = TRUE)

# =========================
# 8. Contribution des variables
# =========================

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# =========================
# 9. Projection des variables qualitatives
# =========================

# # ⚠️ Important : aligner les lignes (mêmes individus après na.omit)
# 
# data_clean <- data_reduit %>%
#   select(ClaimNb, Exposure, VehPower, VehAge, DrivAge, BonusMalus, Density,
#          VehBrand, VehGas, Area, Region) %>%
#   na.omit()

# ACP avec variables qualitatives en supplémentaires
# res.pca.quali <- PCA(data_clean,
#                      scale.unit = TRUE,
#                      quali.sup = 8:11,   # colonnes qualitatives
                     # graph = FALSE)



# =========================
# 10. Visualisation des modalités qualitatives
# =========================

fviz_pca_ind(res.pca,
             habillage = "Area")