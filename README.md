# firstlib

## firstlib: pour analyser des élus municipaux
Un package R pour observer et exploiter les caractéristiques et informations sur les élus municipaux en France.

## Présentation
Avec ce package R, il sera possible d'effectuer des mesures, des statistiques mais aussi de produire des graphiques par rapport à des départements ou communes sélectionnés,
en ayant des données sur des élus municipaux. De plus, il permet également de générer des rapports paramétrables.

## Installation
Pour l'installer, il faudra taper la ligne de commande suivante dans la console R: 
remotes::install_github("Isahrv/firstlib-isaline")

## Exemple d'utilisation
### Chargement de la librairie
library(firstlib)

### Chargement des données disponibles sur le package
df <- firstlib::donnees_elus

### Création d'un dataframe "df_Nantes"
df_Nantes <- df[df$Libellé_de_la_commune == "Nantes",]

### Application de la classe "commune" au dataframe 
df_Nantes <- creer_commune(df_Nantes)

### Résumé d’une commune
summary(df_Nantes)

## Documentation des fonctions  
N'hésitez pas à consulter la [référence des fonctions](reference/index.html) pour voir toutes les fonctionnalités du package disponibles.

## Génération d'un rapport  
Si vous voulez apprendre à générer un rapport d'analyse détaillé, veuillez vous référer à l'article :  
[Générer un rapport d'analyse](articles/generer_un_rapport.html)
