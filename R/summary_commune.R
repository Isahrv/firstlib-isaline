#' @rdname summary
#' @method summary commune
#' @description Résumé des informations pour un dataframe de classe "commune".
#' @param x Un dataframe de classe "commune".
#' @return Un résumé d'information sur une commune, avec le libellé de la commune, le nombre d'élus, la distribution de l'âge des élus, des informations sur l'élu le/la plus agé.e
#' @import dplyr
#' @import lubridate
#' @export
summary.commune <- function(x) {
  # Cette fonction prend un dataframe de classe "commune", correspondant au schéma de validate_schema(), en entrée et retourne un résumé d'informations sur une commune.
  print(paste("Libellé de la commune :", unique(x$Libellé.de.la.commune)))
  print(paste("Nombre d'élus dans la commune :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus de la commune :")
  print(calcul_distribution_age(x))
  print("Élu le/la plus âgé.e de la commune :")
  print(trouver_l_elu_le_plus_age(x))
}
