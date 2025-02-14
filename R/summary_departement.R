#' @rdname summary
#' @method summary departement
#' @description Résumé des informations pour un dataframe de classe "departement".
#' @param x Un dataframe de classe "departement".
#' @return Un résumé d'information sur un département, avec le libellé du département, le nombre de commune, le nombre d'élus, la distribution de l'âge des élus, des informations sur l'élu le/la plus agé.e et l'élu le/la plus jeune, leur commune, le nom de la commune avec la moyenne d'âge la plus faible et avec la moyenne d'âge la plus élevée, la distribution des âges pour ces communes
#' @import dplyr
#' @import lubridate
#' @export
summary.departement <- function(x) {
  # Cette fonction prend un dataframe de classe "departement", correspondant au schéma de validate_schema(), en entrée et retourne un résumé d'informations sur un département.
  print(paste("Nom du département :", unique(x$Libellé.du.département)))
  print(paste("Nombre de commune :", compter_nb_commune(x)))
  print(paste("Nombre d'élus dans le département :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus du département :")
  print(calcul_distribution_age(x))

  print("Élu(e) le/la plus âgé(e) du département :")
  elu_plus_age <- trouver_l_elu_le_plus_age(x)
  print(elu_plus_age)
  print(paste("Commune de l'élu(e) le/la plus âgé(e) :", x$Libellé.de.la.commune[x$Nom.de.l.élu == elu_plus_age$Nom.de.l.élu & x$Prénom.de.l.élu == elu_plus_age$Prénom.de.l.élu]))

  print("Élu(e) le/la plus jeune du département :")
  elu_plus_jeune <- trouver_l_elu_le_plus_jeune(x)
  print(elu_plus_jeune)
  print(paste("Commune de l'élu(e) le/la plus jeune :", x$Libellé.de.la.commune[x$Nom.de.l.élu == elu_plus_jeune$Nom.de.l.élu & x$Prénom.de.l.élu == elu_plus_jeune$Prénom.de.l.élu]))

  age <- x |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)

  moyenne_age_par_commune <- age |>
    group_by(Libellé.de.la.commune) |>
    summarise(Moyenne_Age = mean(Age, na.rm = TRUE), .groups = "drop")

  commune_age_min <- moyenne_age_par_commune |>
    slice(which.min(Moyenne_Age)) |>
    pull(Libellé.de.la.commune)

  commune_age_max <- moyenne_age_par_commune |>
    slice(which.max(Moyenne_Age)) |>
    pull(Libellé.de.la.commune)

  print(paste("Commune avec la moyenne d'âge la plus faible :", commune_age_min))
  print("Distribution des âges pour cette commune :")
  print(calcul_distribution_age(filter(x, Libellé.de.la.commune == commune_age_min)))

  print(paste("Commune avec la moyenne d'âge la plus élevée :", commune_age_max))
  print("Distribution des âges pour cette commune :")
  print(calcul_distribution_age(filter(x, Libellé.de.la.commune == commune_age_max)))
}
