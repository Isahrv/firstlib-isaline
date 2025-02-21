#' Une fonction qui produit un graphique sur le nombre d'élus par code professionnel
#'
#' @description Une fonction pour avoir un graphique en barre horizontal montrant le nombre d'élus par code professionnel pour une commune/un département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return un bar chart horizontal, avec le nombre d'élus sur l'axe des abcisses et le code professionnel sur l'axe des ordonnées
#' @import dplyr
#' @import ggplot2
#' @examples
#' plot_code_professions(df_Nantes)
plot_code_professions <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne un graphique montrant le nombre d'élus par code professionnel par départements/communes.
  validate_schema(df)

  df_counts <- df |>
    count(Code_de_la_catégorie_socio_professionnelle, name = "Nombre")

  bar_chart <- ggplot(df_counts, aes(x = reorder(Code_de_la_catégorie_socio_professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() +
    labs(title = "Nombre d'élus par code professionnel",
         x = "Code professionnel",
         y = "Nombre d'élus") +
    theme_classic()
  print(bar_chart)
}
