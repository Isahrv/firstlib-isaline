#' @rdname plot
#' @method plot commune
#' @description Graphique pour un dataframe de classe "commune".
#' @param x Un dataframe de classe "commune".
#' @return Un diagramme en barre horizontal, avec les codes professionnels des élus en ordonné, et le nombre d'élus sur l'axe des abcisses
#' @import dplyr
#' @import ggplot2
#' @export
plot.commune <- function(df) {
  # Cette fonction prend un dataframe de classe "commune", correspondant au schéma de validate_schema(), en entrée et retourne un graphique montrant la distribution d'élus par rapport aux codes professionnels.
  validate_schema(df)

  df_counts <- df |>
    count(Code.de.la.catégorie.socio.professionnelle, name = "Nombre")

  nom_commune <- unique(df$Libellé.de.la.commune)
  nom_departement <- unique(df$Libellé.du.département)
  nb_elus <- sum(df_counts$Nombre)

  titre_graphique <- paste(nom_commune, "-", nom_departement)
  axe_x <- paste("Libellés des codes professionnels pour les", nb_elus, "élus")

  bar_chart <- ggplot(df_counts, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() +
    labs(title = titre_graphique,
         x = axe_x,
         y = "Nombre d'élus") +
    theme_classic()

  print(bar_chart)
}
