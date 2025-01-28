# Question 4

#' @description Une fonction pour compter le nombre d'élus par ville/département et montrer des informations sur ces élus
#' @param df le dataframe doit contenir les colonnes "Nom.de.l.élu", "Prénom.de.l.élu" et "Date.de.naissance"
#' @return une liste avec chaque élus, son nom, son prénom, sa date de naissance et le nombre de fois qu'il apparaît puis retourne le nombre d'élus uniques
#' @importFrom dplyr
#' @export
#' @examples
#' compter_nombre_d_elus(df_Nantes)
compter_nombre_d_elus <- function(df){
  unique_elus <- df |>
    count(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
  nombre_elus <- sum(unique_elus$n)
  return(list(unique_elus = unique_elus, nombre_elus = nombre_elus))
}

#' @description Une fonction pour compter le nombre d'élus par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'élus uniques
#' @importFrom dplyr
#' @export
#' @examples
#' compter_nb_elus(df_Nantes)
compter_nb_elus <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))

  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

#' @description Une fonction pour compter le nombre d'adjoints par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'adjoints uniques
#' @importFrom stringr
#' @export
#' @examples
#' compter_nombre_d_adjoints(df_Nantes)
compter_nombre_d_adjoints <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))

  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}

#' @description Une fonction pour valider le schéma du dataframe
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return rien mais stop la fonction si le df ne correspond pas
#' @export
validate_schema <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
}

#' @description Une fonction pour trouver l'élu le plus agé par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return le nom, le prénom, et la date de naissance de l'élu le plus agé de la commune/département
#' @importFrom lubridate
#' @export
#' @examples
#' trouver_l_elu_le_plus_age(df_Nantes)
trouver_l_elu_le_plus_age <- function(df){
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance))|>
    slice(which.min(Date.de.naissance))|>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}

# Question 5

library(readr)

chemin <- "C:/Users/isali/OneDrive/Documents/cours/M1 ECAP/S2/R avancé & Git/semaine_5/elus_sample.csv"
donnees_elus <- read_delim(chemin, delim = ";")

library(usethis)

use_data(donnees_elus)

#' Echantillon d'élus
#'
#' Tableau contenant un échantillon de 9753 élus des communes françaises
#'
#' @format
#' Un data frame avec 9753 lignes et 16 colonnes:
#' \describe{
#'   \item{Code du département}{code du département}
#'   \item{Libellé du département}{nom du département}
#'   \item{Code de la collectivité à statut particulier}{code de la collectivité à statut particulier}
#'   \item{Libellé de la collectivité à statut particulier}{nom de la collectivité à statut particulier}
#'   \item{Code de la commune}{code de la commune}
#'   \item{Libellé de la commune}{nom de la commune}
#'   \item{Nom de l'élu}{nom de l'élu}
#'   \item{Prénom de l'élu}{prénom de l'élu}
#'   \item{Code sexe}{"M" si l'élu est un homme, "F" si l'élu est une femme}
#'   \item{Date de naissance}{date de naissance de l'élu au format "jj/mm/aaaa"}
#'   \item{Code de la catégorie socio-professionnelle}{code correspondant à la catégorie socio-professionnelle de l'élu}
#'   \item{Libellé de la catégorie socio-professionnelle}{nom de la catégorie socio-professionnelle de l'élu}
#'   \item{Date de début du mandat}{date de début du mandat au format "jj/mm/aaaa"}
#'   \item{Libellé de la fonction}{nom de la fonction de l'élu}
#'   \item{Date de début de la fonction}{date de début de la fonction au format "jj/mm/aaaa"}
#'   \item{Code nationalité}{code indiquant la nationalité de l'élu ("FR" s'il est français)}
#' }
#' @source https://rnedellec-r-advanced-git.netlify.app/schedule
