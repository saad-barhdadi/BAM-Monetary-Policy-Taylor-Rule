# 🏦 Modélisation de la Fonction de Réaction de Bank Al-Maghrib (2006-2025)

Ce repository présente un projet personnel d'économétrie avancée visant à décrypter la politique monétaire de la banque centrale marocaine (Bank Al-Maghrib). L'objectif est d'évaluer empiriquement le respect de la Règle de Taylor et de modéliser la prise de décision de l'institution face aux chocs macroéconomiques.

## 💡 Problématique
Une simple régression linéaire (MCO) est incapable de capter la réalité institutionnelle d'une banque centrale dont les taux évoluent par paliers stricts et avec une forte inertie. Comment modéliser mathématiquement cette dynamique complexe ?

## ⚙️ Méthodologie Quantitatives
Ce projet déploie un pipeline complet de séries temporelles et de modèles à choix discrets développés sous **R** :

1. **Phase 1 : Approche dynamique continue (Modèle ARDL)**
   * Analyse de stationnarité (Tests ADF).
   * Vérification de la cointégration (Bounds Test de Pesaran).
   * Estimation de la Règle de Taylor de long terme et de la force de rappel (ECM) capturant l'inertie de la banque centrale.

2. **Phase 2 : Approche probabiliste (Modèle Logit Ordonné)**
   * Modélisation de la décision monétaire comme une variable discrète (Baisse, Statu quo, Hausse).
   * Calcul des *Odds Ratios* (Rapports de cotes) et des Effets Marginaux Moyens (AME).
   * Validation prédictive via la matrice de confusion, la courbe ROC multiclasse et le test de calibration de Lipsitz.

## 📊 Résultats Clés
* L'inflation dicte la politique de BAM : une hausse de l'inflation multiplie par **2,5** la probabilité d'un resserrement monétaire.
* Le "statu quo" constitue le régime naturel de l'institution, confirmant une gestion pragmatique axée sur la stabilité du système financier marocain.
* Le modèle Logit Ordonné démontre une excellente capacité prédictive avec un **taux de bon classement de 86,4 %** sur les données historiques.

## 📂 Structure du Repository
* `POLITIQUE_MONETAIRE_TAYLOR.pdf` : Le rapport complet et détaillé de l'étude (37 pages).
* `scripts_R/` : Dossier contenant les codes sources utilisés pour le traitement des données et les modélisations économétriques.

## 🛠️ Stack Technique
* **Langage :** R
* **Packages clés :** `tseries`, `ARDL`, `lmtest`, `marginaleffects`
* **Rédaction & Mise en page :** LaTeX

---
*Projet réalisé par Saad Barhdadi - Juillet 2026.*
