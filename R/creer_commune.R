#' Fonction pour réer un objet de classe "commune"
#'
#' @description Une fonction qui ajoute la classe "commune" à un dataframe
#' @param df Un dataframe avec les informations d'une commune
#' @return Un objet de classe "commune"
#' @export
creer_commune <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema(), en entrée et ajoute la classe "commune" au dataframe
  validate_schema(df)
  class(df) <- c("commune", class(df))
  return(df)
}
