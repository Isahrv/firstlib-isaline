#' Une fonction qui compte le nombre d'élus
#'
#' @description Une fonction pour compter le nombre d'élus uniques par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'élus uniques
#' @import dplyr
#' @examples
#' compter_nb_elus(df_Nantes)
#' 66
compter_nb_elus <- function(df){
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne le nombre d'élus par départements ou communes.
  validate_schema(df)

  df |>
    select(Nom_de_l_élu, Prénom_de_l_élu, Date_de_naissance) |>
    distinct() |>
    nrow()
}
