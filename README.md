# README

## Projet de Prédiction de la Consommation Énergétique en France

### Auteurs

Mohamed Amine GRINI, Samuel MOLANO, Marcos LAHOZ

### Date

9 mars 2025

## Description du Projet

Ce projet vise à prédire la consommation énergétique en France pour la période de 2022 à 2023 en utilisant des modèles de machine learning. Les données couvrent la période de 2013 à 2022 et comprennent plusieurs variables pertinentes.

## Structure du Projet

1. **Visualisation des Données** : Exploration et analyse des tendances.
2. **Choix des Variables** : Sélection des variables les plus pertinentes.
3. **Modélisation** : Comparaison de plusieurs modèles (ARIMA, forêts aléatoires, etc.).
4. **Prédictions** : Prévision de la consommation énergétique.
5. **Discussion** : Analyse des résultats et conclusions.

## Technologies Utilisées

- **Langage** : R
- **Bibliothèques** : `tidyverse`, `lubridate`, `forecast`, `ranger`, `rpart`, `randomForest`, `mgcv`

## Instructions d'Utilisation

### Prérequis

Assurez-vous d'avoir R et RStudio installés.

### Installation

1. Cloner ce dépôt GitHub :
   ```sh
   git clone https://github.com/votre-repo/nom-du-projet.git
   cd nom-du-projet
   ```
2. Installer les dépendances en exécutant :
   ```r
   install.packages(c("tidyverse", "lubridate", "forecast", "ranger", "rpart", "randomForest", "mgcv"))
   ```

### Exécution

Exécuter le script RMarkdown pour générer le rapport :

```r
rmarkdown::render("Rapport_Projet_ModelPred.Rmd")
```

## Auteurs et Contact

Pour toute question, veuillez contacter les auteurs du projet.

## Licence

Ce projet est sous licence MIT. Voir le fichier `LICENSE` pour plus de détails.
