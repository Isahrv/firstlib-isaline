#' Fonction pour réer un objet de classe "departement"
#'
#' @description Une fonction qui ajoute la classe "departement" à un dataframe
#' @param df Un dataframe avec les informations d'un département
#' @return Un objet de classe "departement"
#' @export
creer_departement <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema(), en entrée et ajoute la classe "departement" au dataframe
  validate_schema(df)
  class(df) <- c("departement", class(df))
  return(df)
}
