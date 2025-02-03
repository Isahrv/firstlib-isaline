#' Une fonction qui trouve l'élu le plus agé
#'
#' @description Une fonction pour trouver l'élu le plus agé dans une ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return le nom, le prénom, et la date de naissance de l'élu le plus agé de la commune/département
#' @importFrom lubridate dmy
#' @export
#' @examples
#' trouver_l_elu_le_plus_age(df_Nantes)
trouver_l_elu_le_plus_age <- function(df){
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance))|>
    slice(which.min(Date.de.naissance))|>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}
