#' @rdname plot
#' @method plot departement
#' @description Graphique pour un dataframe de classe "departement".
#' @param x Un dataframe de classe "departement".
#' @return Un diagramme en barre horizontal, avec les 10 codes professionnels des élus les plus représentés en ordonné, et le nombre d'élus sur l'axe des abcisses
#' @import dplyr
#' @import ggplot2
#' @export
plot.departement <- function(df) {
  # Cette fonction prend un dataframe de classe "departement", correspondant au schéma de validate_schema(), en entrée et retourne un graphique montrant la distribution d'élus par rapport aux 10 codes professionnels les plus représentés dans le département.
  validate_schema(df)

  df_counts <- df |>
    count(Code.de.la.catégorie.socio.professionnelle, name = "Nombre") |>
    arrange(desc(Nombre)) |>
    slice_head(n = 10)

  nom_departement <- unique(df$Libellé.du.département)
  nb_communes <- length(unique(df$Libellé.de.la.commune))

  titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
  axe_x_label <- paste("Libellés des 10 codes professionnels les plus représentés pour", nom_departement)

  bar_chart <- ggplot(df_counts, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() +
    labs(title = titre_graphique,
         x = axe_x_label,
         y = "Nombre d'élus") +
    theme_classic()

  print(bar_chart)
}
