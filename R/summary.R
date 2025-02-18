#' Fonction pour résumer des informations d'un dataframe selon sa classe
#'
#' @description Fonction générique qui retourne un résumé d'informations différent selon la classe du dataframe.
#' @param df Un dataframe avec les données à résumer
#' @return Un résumé des données, selon la classe de `df`
#' @seealso \code{\link{summary.commune}}, \code{\link{summary.departement}}
#' @export
summary <- function(df) {
  # Cette fonction générique prend un dataframe en entrée et a comme objectif de retourner un résumé d'informations différent en fonction de la classe du df.
  UseMethod("summary")
}
