
# ETAPE 0 : Nettoyage de l'environnement 
# On efface tout pour éviter les conflits d'objets
rm(list = ls())

# ETAPE 1 : Installation et chargement des packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, psych, ggplot2)

# ETAPE 2 : Importation des données 
# Remplacer par le chemin exact vers ton fichier
chemin_fichier <- "C:/Users/HP/Desktop/Projet_Politique_Monetaire/DATA_CLEAN.xlsx"
data_final <- read_excel(chemin_fichier)

# ETAPE 3 : Définition stricte des variables
variables <- c("Taux directeur", 
               "Quarterly_Morocco_Prices Consumer Price Index", 
               "glissement_annuel")

# ETAPE 4 : Nettoyage et forçage en format numérique
# Cette étape remplace les éventuelles virgules par des points et force le format numérique.
# C'est ce qui résout ton erreur "character vs double".
data_final <- data_final %>%
  mutate(across(all_of(variables), ~ as.numeric(gsub(",", ".", as.character(.)))))

# ETAPE 5 : Statistiques Descriptives
cat("\n--- Statistiques Descriptives (Base Nette) ---\n")
print(describe(data_final[, variables]))

# ETAPE 6 : Préparation pour le graphique 
df_long <- data_final %>%
  pivot_longer(cols = all_of(variables), 
               names_to = "Variable", 
               values_to = "Valeur")
# ETAPE 7 : Création et affichage des graphiques
graphique <- ggplot(df_long, aes(x = Date, y = Valeur, group = Variable)) +
  geom_line(color = "navy", size = 1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Analyse Visuelle des Séries Temporelles Marocaines",
       subtitle = "Période : 2006T4 - 2025T2",
       x = "Trimestres", 
       y = "Valeurs (%)")

print(graphique)

# 1. On s'assure que les packages sont chargés
library(ggplot2)
library(tidyverse)

# 2. On recrée le tableau au format long (si tu ne l'as plus en mémoire)
variables <- c("Taux directeur", "Quarterly_Morocco_Prices Consumer Price Index", "glissement_annuel")

df_long <- data_final %>%
  pivot_longer(cols = all_of(variables), 
               names_to = "Variable", 
               values_to = "Valeur")

# 3. On dessine les courbes de densité (la "forme" de tes données)
graphique_cloche <- ggplot(df_long, aes(x = Valeur, fill = Variable)) +
  geom_density(alpha = 0.6, color = "black") + # geom_density dessine la cloche
  facet_wrap(~ Variable, scales = "free", ncol = 1) +
  theme_minimal() +
  labs(title = "Visualisation de la forme de tes données (Skewness & Kurtosis)",
       subtitle = "Observe vers où la courbe penche et l'épaisseur de ses extrémités",
       x = "Valeurs de la variable (%)", 
       y = "Concentration (Fréquence d'apparition)") +
  theme(legend.position = "none")

print(graphique_cloche)



# --- 1. Préparation (Raccourcir les noms pour éviter les bugs graphiques) ---
library(corrplot)
library(tidyverse)

# On crée un tableau dédié aux graphiques avec des noms courts et propres
donnees_visu <- data_final[, variables]
colnames(donnees_visu) <- c("Taux_Directeur", "Inflation_CPI", "Croissance_PIB")

# --- 2. Matrice de Corrélation ---
matrice_corr <- cor(donnees_visu, use = "complete.obs")

cat("\n--- Matrice de Corrélation ---\n")
print(round(matrice_corr, 2))

# Ajustement des marges pour éviter l'erreur graphique classique de R
par(mar = c(2, 2, 3, 2)) 

# Affichage de la matrice (en mode "number" pour voir les chiffres clairement)
corrplot(matrice_corr, 
         method = "number", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, # Inclinaison du texte plus lisible
         col = colorRampPalette(c("#E46726", "white", "#6D9EC1"))(200),
         title = "Matrice de Corrélation",
         mar = c(0,0,2,0))

# Réinitialisation des marges par défaut pour la suite
par(mar = c(5, 4, 4, 2) + 0.1) 

# --- 3. Détection des Outliers avec les Boxplots ---
# On passe au format long avec nos nouveaux noms courts
df_long_visu <- donnees_visu %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Valeur")

# Création du Boxplot avec un thème renforcé
graphique_boxplot <- ggplot(df_long_visu, aes(x = Variable, y = Valeur, fill = Variable)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 3) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  theme_bw() + # Thème avec des bordures claires
  labs(title = "Détection des Valeurs Aberrantes (Boxplots)",
       subtitle = "Les points rouges représentent les chocs exceptionnels",
       x = "", 
       y = "Valeurs (%)") +
  theme(legend.position = "none",
        axis.text.x = element_blank(), # Enlève le texte inutile en bas
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 11, face = "bold")) # Noms des variables en gras

print(graphique_boxplot)



# ETAPE 8 : Tests de Stationnarité (ADF) 
# 1. Chargement du package requis pour les séries temporelles
if (!require("tseries")) install.packages("tseries")
library(tseries)

# 2. Lancement des tests ADF en retirant les éventuelles valeurs manquantes (na.omit)
cat("\n======================================================\n")
cat("--- Test ADF : Taux Directeur ---\n")
print(adf.test(na.omit(data_final$`Taux directeur`)))

cat("\n======================================================\n")
cat("--- Test ADF : Inflation (CPI) ---\n")
print(adf.test(na.omit(data_final$`Quarterly_Morocco_Prices Consumer Price Index`)))

cat("\n======================================================\n")
cat("--- Test ADF : Croissance (PIB) ---\n")
print(adf.test(na.omit(data_final$glissement_annuel)))

# ETAPE 9 : Tests ADF en Différence Première 

cat("\n======================================================\n")
cat("--- Test ADF : Différence Taux Directeur ---\n")
print(adf.test(na.omit(diff(data_final$`Taux directeur`))))

cat("\n======================================================\n")
cat("--- Test ADF : Différence Inflation (CPI) ---\n")
print(adf.test(na.omit(diff(data_final$`Quarterly_Morocco_Prices Consumer Price Index`))))

cat("\n======================================================\n")
cat("--- Test ADF : Différence Croissance (PIB) ---\n")
print(adf.test(na.omit(diff(data_final$glissement_annuel))))

# GÉNÉRATION DU GRAPHIQUE DE STATIONNARITÉ 

# 1. Ouvrir une fenêtre graphique de haute résolution (optionnel, pour l'export)
# Tu peux retirer le # de la ligne ci-dessous si tu veux sauvegarder l'image directement
# png("stationnarite_plot.png", width = 3000, height = 2400, res = 300)

# 2. Configuration de la grille (3 lignes, 2 colonnes) et des marges
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

# 3. Tracé : Taux Directeur
plot(data_ardl$Taux_Directeur, type = "l", col = "darkblue", lwd = 2,
     main = "Taux Directeur (En niveau)", ylab = "Niveau (%)", xlab = "")
grid()
plot(diff(data_ardl$Taux_Directeur), type = "l", col = "darkcyan", lwd = 2,
     main = "Taux Directeur (En différence 1ère)", ylab = "Variation", xlab = "")
grid()

# 4. Tracé : Inflation CPI
plot(data_ardl$Inflation_CPI, type = "l", col = "darkred", lwd = 2,
     main = "Inflation CPI (En niveau)", ylab = "Niveau (%)", xlab = "")
grid()
plot(diff(data_ardl$Inflation_CPI), type = "l", col = "darkorange", lwd = 2,
     main = "Inflation CPI (En différence 1ère)", ylab = "Variation", xlab = "")
grid()

# 5. Tracé : Croissance PIB
plot(data_ardl$Croissance_PIB, type = "l", col = "purple", lwd = 2,
     main = "Croissance PIB (En niveau)", ylab = "Niveau (%)", xlab = "Trimestres")
grid()
plot(diff(data_ardl$Croissance_PIB), type = "l", col = "magenta", lwd = 2,
     main = "Croissance PIB (En différence 1ère)", ylab = "Variation", xlab = "Trimestres")
grid()

# dev.off() # À décommenter si tu as utilisé la ligne png() au début



# ETAPE 10 : Le Modèle Naïf (Régression MCO) et ses Limites

# 1. Chargement du package pour les tests de diagnostic économétrique
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)
# (Le package tseries est déjà chargé grâce à l'étape ADF, on l'utilisera pour Jarque-Bera)

# 2. Construction du modèle MCO (La Règle de Taylor basique)
modele_mco <- lm(`Taux directeur` ~ `Quarterly_Morocco_Prices Consumer Price Index` + glissement_annuel, data = data_final)

# 3. Affichage des résultats "apparents" du modèle
cat("\n======================================================\n")
cat("--- RÉSULTATS DU MODÈLE MCO (LA RÉGRESSION APPARENTE) ---\n")
print(summary(modele_mco))

# 4. Tests de diagnostic sur les erreurs (Les Résidus)
cat("\n======================================================\n")
cat("--- 1. TEST DE NORMALITÉ DES ERREURS (Jarque-Bera) ---\n")
# Objectif : Prouver que les erreurs ne sont pas normales
print(jarque.bera.test(residuals(modele_mco)))

cat("\n======================================================\n")
cat("--- 2. TEST D'AUTOCORRÉLATION (Breusch-Godfrey) ---\n")
# Objectif : Prouver que le modèle ignore l'inertie de la banque centrale
print(bgtest(modele_mco, order = 4)) # On teste sur 4 trimestres (1 an)

cat("\n======================================================\n")
cat("--- 3. TEST D'HÉTÉROSCÉDASTICITÉ (Breusch-Pagan) ---\n")
# Objectif : Prouver que la variance des chocs n'est pas constante
print(bptest(modele_mco))

# ETAPE 11 (CORRIGÉE) : La Modélisation Dynamique (ARDL)

# 1. Préparation d'un jeu de données avec des noms "sécurisés" pour le package ARDL
data_ardl <- data_final

# On renomme les trois colonnes proprement (sans espaces)
colnames(data_ardl)[colnames(data_ardl) == "Taux directeur"] <- "Taux_Directeur"
colnames(data_ardl)[colnames(data_ardl) == "Quarterly_Morocco_Prices Consumer Price Index"] <- "Inflation_CPI"
colnames(data_ardl)[colnames(data_ardl) == "glissement_annuel"] <- "Croissance_PIB"

# 2. Chargement du package ARDL
library(ARDL)

# 3. Recherche automatique des meilleurs décalages (Lags)
# On utilise maintenant nos variables propres : Taux_Directeur, Inflation_CPI, Croissance_PIB
modeles_ardl <- auto_ardl(Taux_Directeur ~ Inflation_CPI + Croissance_PIB,
                          data = data_ardl,
                          max_order = 4,
                          selection = "AIC")

# 4. Extraction du meilleur modèle trouvé par l'algorithme
meilleur_ardl <- modeles_ardl$best_model

# 5. Affichage des retards (Lags) optimaux
cat("\n======================================================\n")
cat("--- 1. L'ORDRE DU MODÈLE (LES DÉCALAGES OPTIMAUX) ---\n")
print(modeles_ardl$top_orders[1, ]) 

# 6. Affichage des résultats du modèle complet
cat("\n======================================================\n")
cat("--- 2. RÉSULTATS DU MEILLEUR MODÈLE ARDL ---\n")
print(summary(meilleur_ardl))


# ETAPE 12: Test de Cointégration (F-Bounds Test)

# 1. Lancement du test F aux bornes de Pesaran
test_coint <- bounds_f_test(meilleur_ardl, case = 3)

# 2. Affichage des résultats pour le Long Terme
cat("\n======================================================\n")
cat("--- TEST AUX BORNES DE PESARAN (F-STATISTIC) ---\n")
print(test_coint)



# ETAPE 13 : L'Équation de Long Terme et la Force de Rappel (ECM) 

# 1. Calcul des coefficients de l'équilibre de long terme
cat("\n======================================================\n")
cat("--- 1. LA RÈGLE DE TAYLOR DE LONG TERME ---\n")
# La fonction multipliers calcule la relation pure sur 20 ans
print(multipliers(meilleur_ardl))

# 2. Modèle à Correction d'Erreur (Vitesse d'ajustement)
cat("\n======================================================\n")
cat("--- 2. VITESSE DE RETOUR À L'ÉQUILIBRE (MODÈLE ECM) ---\n")
# La fonction uecm crée le modèle qui montre comment BAM réagit aux déséquilibres
modele_ecm <- uecm(meilleur_ardl)
print(summary(modele_ecm))


# --- VISUALISATION DE LA CONVERGENCE (L'ÉCART À L'ÉQUILIBRE) ---
# 1. Extraction des résidus du modèle ECM (qui représentent l'écart à l'équilibre)
residus_ecm <- residuals(modele_ecm)

# 2. Création du graphique de retour à l'équilibre
plot(residus_ecm, type = "l", col = "navy", lwd = 2,
     main = "Dynamique de convergence vers l'équilibre de long terme (ECT)",
     ylab = "Écart à l'équilibre", xlab = "Temps (Trimestres)",
     las = 1)

# 3. Ajout de la ligne zéro (Le sentier d'équilibre parfait)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Ajout d'une légende textuelle pour le jury
legend("topright", legend = c("Déviations de court terme", "Sentier d'équilibre (Zéro)"),
       col = c("navy", "red"), lty = c(1, 2), lwd = 2, cex = 0.8)


# --- VISUALISATION AVANCÉE DE LA CONVERGENCE (ECT) AVEC GGPLOT2 ---

# 1. Extraction des résidus du modèle ECM (Écart à l'équilibre)
residus_ecm <- residuals(modele_ecm)

# 2. Création d'un DataFrame propre pour ggplot
# L'ARDL(1,0,2) consomme les 2 premiers trimestres pour ses retards, 
# la série commence donc à la 3ème observation.
dates_ecm <- data_final$Date[3:nrow(data_final)]

df_ect <- data.frame(
  Trimestre = dates_ecm,
  ECT = as.numeric(residus_ecm)
)

# 3. Création du graphique haute qualité
graphique_ect <- ggplot(df_ect, aes(x = Trimestre, y = ECT, group = 1)) +
  # Zone d'ombre (ribbon) pour souligner visuellement l'écart par rapport à zéro
  geom_ribbon(aes(ymin = 0, ymax = ECT), fill = "steelblue", alpha = 0.3) +
  
  # Ligne principale des résidus
  geom_line(color = "navy", linewidth = 1) +
  
  # Ligne d'équilibre parfait (Zéro)
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1.2) +
  
  # Esthétique générale et thème
  theme_minimal() +
  labs(
    title = "Dynamique de convergence vers l'équilibre de long terme",
    subtitle = "Le Terme à Correction d'Erreur (ECT) illustre la force de rappel de Bank Al-Maghrib",
    x = "Trimestres",
    y = "Écart à l'équilibre (Résidus)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "darkgray", size = 11),
    panel.grid.minor = element_blank()
  ) +
  
  # Annotations explicatives directes sur le graphique
  annotate("text", x = 5, y = max(df_ect$ECT) * 0.9, 
           label = "Déséquilibres transitoires (Chocs)", 
           color = "navy", fontface = "bold", hjust = 0, size = 4) +
  annotate("text", x = 5, y = -0.02, 
           label = "Sentier d'équilibre macroéconomique", 
           color = "red", fontface = "italic", hjust = 0, size = 4)

# Affichage du graphique
print(graphique_ect)

dev.off()

# --- 2. TEST OLS-CUSUM (Stabilité globale des coefficients) ---
cusum_res <- efp(residuals(modele_ecm) ~ 1, type = "OLS-CUSUM")

# --- 3. TEST Rec-CUSUM (Stabilité récursive) ---
# Correction de l'argument "RE-CUSUM" en "Rec-CUSUM"
cusumsq_res <- efp(residuals(modele_ecm) ~ 1, type = "Rec-CUSUM")

# --- 4. AFFICHAGE ET EXPORTATION DES GRAPHIQUES ---
# Ouverture du fichier image
png("cusum_graph.png", width = 800, height = 400)

# Configuration pour avoir 1 ligne et 2 colonnes (les deux graphiques côte à côte)
par(mfrow = c(1, 2)) 

# Tracé des graphiques
plot(cusum_res, main = "Test OLS-CUSUM (Stabilité globale)")
plot(cusumsq_res, main = "Test Rec-CUSUM (Stabilité récursive)")

# Fermeture et sauvegarde du fichier image
dev.off()

getwd()

# --- ETAPE 14 : Tests de Limites (Justification pour la Phase 2) ---

# 1. Chargement du package lmtest (Normalement déjà chargé à l'Etape 10)
library(lmtest)

cat("\n======================================================\n")
cat("--- 1. TEST RESET DE RAMSEY (Erreur de spécification linéaire) ---\n")
# Objectif : Prouver que l'équation linéaire ARDL manque une dynamique asymétrique.
# On applique le test directement sur ton objet 'meilleur_ardl' généré à l'Etape 11
test_reset <- resettest(meilleur_ardl, power = 2:3, type = "fitted")
print(test_reset)


cat("\n======================================================\n")
cat("--- 2. TEST DE SHAPIRO-WILK (Preuve de la nature discrète) ---\n")
# Objectif : Prouver que les décisions de BAM se font par paliers et non en continu.
# On calcule la différence (Delta) sur ta variable renommée proprement
delta_taux_propre <- na.omit(diff(data_ardl$Taux_Directeur))
test_shapiro <- shapiro.test(delta_taux_propre)
print(test_shapiro)

c

# 1. Chargement des packages (tidyverse contient dplyr pour la manipulation)
library(tidyverse)

# 2. Création d'une nouvelle base dédiée à la Phase 2
data_logit <- data_final

# Nettoyage des noms de colonnes si ce n'est pas déjà fait sur data_final
colnames(data_logit)[colnames(data_logit) == "Taux directeur"] <- "Taux_Directeur"
colnames(data_logit)[colnames(data_logit) == "Quarterly_Morocco_Prices Consumer Price Index"] <- "Inflation_CPI"
colnames(data_logit)[colnames(data_logit) == "glissement_annuel"] <- "Croissance_PIB"

# 3. Création de la variable discrète "Decision_BAM"
data_logit <- data_logit %>%
  mutate(
    # Calcul de la variation par rapport au trimestre précédent
    Delta_Taux = Taux_Directeur - lag(Taux_Directeur),
    
    # Codage catégoriel des décisions
    Decision = case_when(
      Delta_Taux > 0 ~ "Hausse",
      Delta_Taux < 0 ~ "Baisse",
      Delta_Taux == 0 ~ "Statu quo"
    )
  ) %>%
  # Suppression de la première ligne (devenue NA à cause du lag)
  drop_na(Decision)

# 4. Transformation en facteur ORDONNÉ (Crucial pour le modèle ordonné)
# L'ordre mathématique est très important : Baisse < Statu quo < Hausse
data_logit$Decision <- factor(data_logit$Decision, 
                              levels = c("Baisse", "Statu quo", "Hausse"), 
                              ordered = TRUE)

# 5. Vérification de la distribution des choix de BAM
cat("\n--- Distribution des décisions de Bank Al-Maghrib ---\n")
frequences <- table(data_logit$Decision)
print(frequences)
cat("\n--- En pourcentages ---\n")
print(prop.table(frequences) * 100)

# ==============================================================================
# ESTIMATION DU MODÈLE LOGIT BINAIRE (Avec la fonction glm)
# ==============================================================================

library(tidyverse)

# 1. Transformation de la variable en Binaire (0 et 1)
# 0 = La banque ne fait rien (Statu quo)
# 1 = La banque agit (Hausse ou Baisse)
data_logit_binaire <- data_logit %>%
  mutate(
    Decision_Binaire = ifelse(Decision == "Statu quo", 0, 1)
  )

# 2. Estimation par la fonction classique glm()
# On précise family = binomial(link = "logit") pour indiquer un Logit
modele_glm <- glm(Decision_Binaire ~ Inflation_CPI + Croissance_PIB, 
                  data = data_logit_binaire, 
                  family = binomial(link = "logit"))

# 3. L'affichage complet façon MCO (Celui que tu cherchais !)
cat("\n======================================================\n")
cat("--- RÉSUMÉ COMPLET DU MODÈLE LOGIT BINAIRE ---\n")
print(summary(modele_glm))


library(DescTools)
PseudoR2(modele_glm, which = "McFadden")

# ==============================================================================
# VISUALISATION DES COEFFICIENTS (FOREST PLOT)
# ==============================================================================

# Chargement des packages nécessaires
if (!require("broom")) install.packages("broom")
if (!require("ggplot2")) install.packages("ggplot2")
library(broom)
library(ggplot2)

# Extraction des coefficients et de leurs intervalles de confiance à 95%
resultats_tidy <- tidy(modele_logit, conf.int = TRUE) %>%
  # On ne garde que les variables explicatives (on enlève les constantes/seuils)
  filter(term %in% c("Inflation_CPI", "Croissance_PIB")) %>%
  # Renommer proprement pour le graphique
  mutate(term = recode(term, 
                       "Inflation_CPI" = "Inflation (CPI)",
                       "Croissance_PIB" = "Croissance (PIB)"))

# Création du graphique "Dot-and-Whisker"
ggplot(resultats_tidy, aes(x = estimate, y = term, color = term)) +
  # La fameuse ligne rouge du Zéro
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  # Les points et les lignes (Intervalles de confiance)
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 1, linewidth = 1.2) +
  scale_color_manual(values = c("Inflation (CPI)" = "#1f77b4", "Croissance (PIB)" = "#7f7f7f")) +
  labs(title = "Sensibilité des décisions monétaires aux fondamentaux",
       subtitle = "Coefficients estimés et intervalles de confiance (95%)",
       x = "Impact sur la probabilité de hausse des taux (Log-Odds)",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 12))


# ==============================================================================
# CALCUL DES EFFETS MARGINAUX (MODÈLE LOGIT ORDONNÉ)
# ==============================================================================

# Chargement du package de référence pour les effets marginaux
if (!require("marginaleffects")) install.packages("marginaleffects")
library(marginaleffects)

cat("\n====================================================================\n")
cat("          EFFETS MARGINAUX MOYENS (Average Marginal Effects)        \n")
cat("====================================================================\n")

# Calcul des effets marginaux moyens pour chaque modalité de la décision
effets_marginaux <- avg_slopes(modele_logit)

# Affichage des résultats
print(effets_marginaux)

# ==============================================================================
# TESTS DE ROBUSTESSE : SIGNIFICATIVITÉ GLOBALE, PSEUDO R² ET AIC
# ==============================================================================
# Rappel : chargement du package nécessaire pour le Logit Ordonné
library(MASS) 

# Étape 1 : Test de significativité globale (Rapport de Vraisemblance)
# Modèle nul (seuils uniquement, aucune variable explicative)
logit_null <- polr(Decision ~ 1, data = data_logit, Hess = TRUE)

# Test du rapport de vraisemblance (Likelihood Ratio Test)
anova(logit_null, modele_logit, test = "Chisq")

# Questions d'interprétation pour ton rapport :
# 1. Que vaut la déviance (Residual Deviance) du modèle nul face au modèle complet ?
# 2. La p-value (Pr(Chi)) rejette-t-elle l'hypothèse nulle ? Le modèle est-il globalement significatif ?


# Étape 2 : Pseudo R² de McFadden
# Calcul manuel du pseudo R² via la comparaison des log-vraisemblances
ll_null <- logLik(logit_null)
ll_model <- logLik(modele_logit)
pseudo_r2 <- 1 - as.numeric(ll_model / ll_null)
cat("Pseudo R² de McFadden :", round(pseudo_r2, 4), "\n")

# Questions d'interprétation pour ton rapport :
# 1. Ce R² (qui devrait tourner autour de 0.1885) est-il un bon score pour des choix discrets ?
# 2. Comment justifier la différence de lecture entre ce Pseudo R² et le R² classique des MCO de ta Phase 1 ?


# Étape 3 : Comparaison de modèles (Critère AIC)
# Estimer un modèle réduit avec uniquement l'inflation (puisque la croissance était l'objectif secondaire)
logit_reduit <- polr(Decision ~ Inflation_CPI, data = data_logit, Hess = TRUE)

# Comparer les critères d'information (AIC)
AIC(logit_reduit, modele_logit)

# Question d'interprétation pour ton rapport :
# Quel modèle affiche l'AIC le plus faible ? Le modèle complet est-il préférable malgré la pénalité liée à l'ajout de la variable croissance ?


# ==============================================================================
# TESTS DE SPÉCIFICATION ET DE VALIDATION (LOGIT ORDONNÉ)
# ==============================================================================
library(MASS)
library(lmtest)

# Matrice de confusion 
cat("\n--- MATRICE DE CONFUSION ---\n")
# Prédictions des classes (Baisse, Statu quo, Hausse)
pred_class <- predict(modele_logit, type = "class")
matrice_conf <- table(Reel = data_logit$Decision, Pred = pred_class)
print(matrice_conf)

# Taux de bon classement (Accuracy) global
taux_bon_classement <- sum(diag(matrice_conf)) / sum(matrice_conf)
cat("Taux de bon classement (Accuracy) :", round(taux_bon_classement, 4), "\n")
# Note pour la rédaction : En multiclasse, on se concentre sur l'Accuracy globale plutôt que sur la sensibilité/spécificité.


# AUC (Version Multiclasse) ---
cat("\n--- AUC MULTICLASSE ---\n")
if (!require("pROC")) install.packages("pROC")
library(pROC)

# Pour un modèle à 3 choix, on calcule une AUC Multiclasse (Hand & Till, 2001)
roc_multi <- multiclass.roc(data_logit$Decision, as.numeric(pred_class))
cat("AUC Multiclasse :", round(roc_multi$auc, 4), "\n")
# Interprétation : Un score > 0.70 indique un bon pouvoir discriminant.


# Le Linktest 
cat("\n--- LINKTEST (Test de spécification) ---\n")
# On récupère le prédicteur linéaire (X*Beta) du modèle polr
y_pred <- modele_logit$lp

# On réestime le modèle avec le prédicteur et son carré
linktest_model <- polr(data_logit$Decision ~ y_pred + I(y_pred^2), Hess = TRUE)
print(coeftest(linktest_model))
# Règle de décision : 
# 1. Le terme y_pred (le prédicteur linéaire) DOIT être significatif.
# 2. Le terme I(y_pred^2) ne DOIT PAS être significatif. S'il l'est, cela signifie qu'il manque des variables ou qu'il y a un problème de spécification.


# Test d'adéquation (Équivalent Hosmer-Lemeshow) ---
cat("\n--- TEST DE LIPSITZ (Goodness-of-fit ordinal) ---\n")
# Le test de Lipsitz est la version académique du Hosmer-Lemeshow pour les Logits Ordonnés
if (!require("generalhoslem")) install.packages("generalhoslem")
library(generalhoslem)

lipsitz_test <- lipsitz.test(modele_logit)
print(lipsitz_test)
# Règle de décision : Contrairement aux autres tests, ici l'hypothèse nulle (H0) est "Le modèle est bien calibré". 
# On veut donc une p-value > 0.05 !


# Test d'une spécification alternative ---
cat("\n--- TEST DE NON-LINÉARITÉ (Inflation au carré) ---\n")
# On vérifie si une forte inflation a un effet exponentiel en ajoutant le terme au carré
logit_alt <- polr(Decision ~ Inflation_CPI + I(Inflation_CPI^2) + Croissance_PIB,
                  data = data_logit, Hess = TRUE)

# On compare le modèle initial et le modèle avec le terme au carré
anova(modele_logit, logit_alt)
# Règle de décision : 
# Si la p-value de l'ANOVA est > 0.05, l'ajout du carré n'améliore pas significativement le modèle. 
# Cela valide que ta spécification initiale (linéaire) était la bonne !


# ==============================================================================
# VISUALISATIONS (APPROXIMATION BINAIRE POUR COURBE ROC ET CALIBRATION)
# ==============================================================================
# Chargement des packages
library(pROC)
library(ggplot2)
library(dplyr)

# 1. Création d'une variable binaire (1 = Action de BAM, 0 = Statu quo)
data_logit$Intervention <- ifelse(data_logit$Decision == "Statu quo", 0, 1)

# 2. Modèle binaire exclusif pour la génération des graphiques
modele_visu <- glm(Intervention ~ Inflation_CPI + Croissance_PIB, 
                   data = data_logit, family = binomial(link = "logit"))

# --- GRAPH 1 : COURBE ROC ---
# Sauvegarde cette image sous le nom "roc_curve.png"
roc_obj <- roc(data_logit$Intervention, fitted(modele_visu))
plot(roc_obj, main = "Courbe ROC (Intervention vs Statu Quo)", 
     col = "darkred", lwd = 3, print.auc = TRUE)

# --- GRAPH 2 : GRAPHIQUE DE CALIBRATION ---
# Sauvegarde cette image sous le nom "calibration.png"
data_logit$prob_prevue <- fitted(modele_visu)
data_logit$prob_group <- cut(data_logit$prob_prevue, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

calibration <- data_logit %>%
  group_by(prob_group) %>%
  summarise(n = n(),
            prob_prevue = mean(prob_prevue),
            prob_observee = mean(Intervention))

ggplot(calibration, aes(x = prob_prevue, y = prob_observee)) +
  geom_point(size = 4, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue", linewidth = 1.2) +
  labs(title = "Graphique de calibration de la politique monétaire",
       subtitle = "Probabilités prévues vs observées (Intervention vs Statu Quo)",
       x = "Probabilité d'intervention prévue par le modèle", 
       y = "Fréquence réelle d'intervention observée") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

