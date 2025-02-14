#' Une fonction qui calcule la distribution de l'âge des élus
#'
#' @description Une fonction pour avoir la distribution de l'âge des élus (quantiles) dans une ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma
#' @return l'âge des élus de la ville/ du département correspondant aux quantiles
#' @importFrom lubridate dmy
#' @import dplyr
#' @examples
#' calcul_distribution_age(df_Nantes) # Devrait retourner un tableau avec les quantiles en ligne et la distribution de l'âge des élus sur la colonne
calcul_distribution_age <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne la distribution de l'âge des élus dans un département/une commune.
  validate_schema(df)

  df <- df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)

  quantiles <- quantile(df$Age, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)

  return(quantiles)
}
