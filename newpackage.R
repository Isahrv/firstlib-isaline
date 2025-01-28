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
