#' Une fonction qui compte le nombre d'adjoints
#'
#' @description Une fonction pour compter le nombre d'adjoints uniques par ville/département
#' @param df le dataframe doit contenir les colonnes correspondantes au schéma décrit
#' @return le nombre d'adjoints uniques
#' @importFrom stringr str_detect
#' @export
#' @examples
#' compter_nombre_d_adjoints(df_Nantes) # Devrait retourner 2 par exemple
compter_nombre_d_adjoints <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))

  sum(!is.na(df$Libellé.de.la.fonction) & str_detect(tolower(df$Libellé.de.la.fonction), "adjoint"))
}
