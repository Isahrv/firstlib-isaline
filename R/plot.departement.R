#' @rdname plot
#' @method plot departement
#' @description Graphique pour un dataframe de classe "departement".
#' @param x Un dataframe de classe "departement".
#' @return Un diagramme en barre horizontal, avec les 10 codes professionnels des élus les plus représentés en ordonné, et le nombre d'élus sur l'axe des abcisses
#' @import dplyr
#' @import ggplot2
#' @export
plot.departement <- function(x) {
  # Cette fonction prend un dataframe de classe "departement", correspondant au schéma de validate_schema(), en entrée et retourne un graphique montrant la distribution d'élus par rapport aux 10 codes professionnels les plus représentés dans le département.
  validate_schema(x)

  df_counts <- x |>
    count(Code_de_la_catégorie_socio_professionnelle, name = "Nombre") |>
    arrange(desc(Nombre)) |>
    slice_head(n = 10)

  nom_departement <- unique(x$Libellé_du_département)
  nb_communes <- length(unique(x$Libellé_de_la_commune))

  titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
  axe_x_label <- paste("Libellés des 10 codes professionnels les plus représentés pour", nom_departement)

  bar_chart <- ggplot(df_counts, aes(x = reorder(Code_de_la_catégorie_socio_professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() +
    labs(title = titre_graphique,
         x = axe_x_label,
         y = "Nombre d'élus") +
    theme_classic()

  print(bar_chart)
}
