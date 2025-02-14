#' Une fonction qui trouve le nombre de commune d'un département
#'
#' @description Une fonction pour calculer le nombre de commune dans un département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return le nombre de commune du département
#' @import dplyr
#' @examples
#' compter_nb_commune(df_Loire_Atlantique) # Devrait retourner 207
compter_nb_commune <- function(df){
  # Cette fonction prend un dataframe sur un département, correspondant au schéma de validate_schema(), en entrée et retourne le nombre de commune dans un département.
  validate_schema(df)

  df |>
    select(Libellé.de.la.commune) |>
    distinct() |>
    nrow()
}
