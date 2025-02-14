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
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)|>
    slice(which.min(Age)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Age)
}
