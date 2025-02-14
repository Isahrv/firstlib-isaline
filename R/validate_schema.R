#' Une fonction qui valide le schéma du df
#'
#' @description Une fonction pour valider le fait que le schéma du dataframe suive le schéma souhaité
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return rien mais stop la fonction si le df ne correspond pas
#'
validate_schema <- function(df){
  # Cette fonction permet d'arrêter le code si jamais le df en entré ne correspond pas au schéma indiqué ci-dessous.
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
}
