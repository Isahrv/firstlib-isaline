#' Une fonction qui compte le nombre d'adjoints
#'
#' @description Une fonction pour compter le nombre d'adjoints uniques par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'adjoints uniques
#' @importFrom stringr str_detect
#' @examples
#' compter_nombre_d_adjoints(df_Nantes) # Devrait retourner 26
compter_nombre_d_adjoints <- function(df){
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne le nombre d'adjoints par départements ou communes.
  validate_schema(df)

  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}
