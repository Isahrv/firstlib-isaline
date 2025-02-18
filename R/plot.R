#' Fonction pour produire un graphique par rapport à un dataframe selon sa classe
#'
#' @description Fonction générique qui retourne un graphique différent selon la classe du dataframe.
#' @param df Un dataframe avec les données nécessaires pour le graphique
#' @return Un graphique, selon la classe de `df`
#' @seealso \code{\link{plot.commune}}, \code{\link{plot.departement}}
plot <- function(df) {
  # Cette fonction générique prend un dataframe en entrée et a comme objectif de retourner un graphique différent en fonction de la classe du df.
  UseMethod("plot")
}
