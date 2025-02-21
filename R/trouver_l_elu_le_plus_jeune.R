#' Une fonction qui trouve l'élu le plus jeune
#'
#' @description Une fonction pour trouver l'élu le plus jeune dans une ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return le nom, le prénom, et l'âge de l'élu le plus jeune de la commune/département
#' @importFrom lubridate dmy
#' @import dplyr
#' @examples
#' trouver_l_elu_le_plus_jeune(df_Nantes)
trouver_l_elu_le_plus_jeune <- function(df) {
  # Cette fonction prend un dataframe  correspondant au schéma de validate_schema() en entrée et retourne le nom, le prénom et l'âge de l'élu le plus jeune dans un département/une commune.
  validate_schema(df)

  df |>
    mutate(Date_de_naissance = dmy(Date_de_naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date_de_naissance, units = "days")) %/% 365)|>
    slice(which.min(Age)) |>
    select(Nom_de_l_élu, Prénom_de_l_élu, Age)
}
