#' Une fonction qui valide le schéma du df
#'
#' @description Une fonction pour valider le fait que le schéma du dataframe suive le schéma souhaité
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return rien mais stop la fonction si le df ne correspond pas
#'
validate_schema <- function(df){
  # Cette fonction permet d'arrêter le code si jamais le df en entré ne correspond pas au schéma indiqué ci-dessous.
  schema <- c("Code_du_département", "Libellé_du_département", "Code_de_la_collectivité_à_statut_particulier", "Libellé_de_la_collectivité_à_statut_particulier", "Code_de_la_commune", "Libellé_de_la_commune", "Nom_de_l_élu", "Prénom_de_l_élu", "Code_sexe", "Date_de_naissance", "Code_de_la_catégorie_socio_professionnelle", "Libellé_de_la_catégorie_socio_professionnelle", "Date_de_début_du_mandat", "Libellé_de_la_fonction", "Date_de_début_de_la_fonction", "Code_nationalité")
  stopifnot(identical(colnames(df), schema))
}
