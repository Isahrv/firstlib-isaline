#' @rdname summary
#' @method summary departement
#' @description Résumé des informations pour un dataframe de classe "departement".
#' @param x Un dataframe de classe "departement".
#' @return Un résumé des informations sur un département.
#' @import dplyr
#' @import lubridate
#' @export
summary.departement <- function(x) {
  print(paste("Nom du département :", unique(x$Libellé_du_département)))
  print(paste("Nombre de commune :", compter_nb_commune(x)))
  print(paste("Nombre d'élus dans le département :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus du département :")
  print(calcul_distribution_age(x))

  elu_plus_age <- trouver_l_elu_le_plus_age(x)
  cat("\nÉlu(e) le/la plus âgé(e) du département :\n")
  print(elu_plus_age)

  commune_plus_age <- x |>
    filter(Nom_de_l_élu == elu_plus_age$Nom_de_l_élu, Prénom_de_l_élu == elu_plus_age$Prénom_de_l_élu)|>
    pull(Libellé_de_la_commune) |>
    unique()

  cat("Commune de l'élu(e) le/la plus âgé(e) :", commune_plus_age, "\n")

  elu_plus_jeune <- trouver_l_elu_le_plus_jeune(x)
  cat("\nÉlu(e) le/la plus jeune du département :\n")
  print(elu_plus_jeune)

  commune_plus_jeune <- x |>
    filter(Nom_de_l_élu == elu_plus_jeune$Nom_de_l_élu, Prénom_de_l_élu == elu_plus_jeune$Prénom_de_l_élu)|>
    pull(Libellé_de_la_commune) |>
    unique()

  cat("Commune de l'élu(e) le/la plus jeune :", commune_plus_jeune, "\n")

  age <- x |>
    mutate(Date_de_naissance = dmy(Date_de_naissance),
           Age = as.numeric(difftime(Sys.Date(), Date_de_naissance, units = "days")) %/% 365)

  moyenne_age_par_commune <- age |>
    group_by(Libellé_de_la_commune) |>
    summarise(Moyenne_Age = mean(Age, na.rm = TRUE), .groups = "drop")

  commune_age_min <- moyenne_age_par_commune |>
    slice_min(Moyenne_Age) |>
    pull(Libellé_de_la_commune)

  commune_age_max <- moyenne_age_par_commune |>
    slice_max(Moyenne_Age) |>
    pull(Libellé_de_la_commune)

  cat("\nCommune avec la moyenne d'âge la plus faible :", commune_age_min, "\n")
  cat("Distribution des âges pour cette commune :\n")
  print(calcul_distribution_age(filter(x, Libellé_de_la_commune == commune_age_min)))

  cat("\nCommune avec la moyenne d'âge la plus élevée :", commune_age_max, "\n")
  cat("Distribution des âges pour cette commune :\n")
  print(calcul_distribution_age(filter(x, Libellé_de_la_commune == commune_age_max)))
}
