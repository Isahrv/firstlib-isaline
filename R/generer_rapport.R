#' Génère un rapport Quarto pour une commune ou un département
#'
#' @description Une fonction qui génère un rapport Quarto au format html correspondant à la commune et au département choisis. Si un document avec le même nom et au même endroit existe déjà, au nom de l'ancien rapport sera ajouté "old" ainsi que la date à laquelle le rapport à été généré.
#' @param commune Code de la commune
#' @param departement Code du département
#' @param output Chemin du fichier de sortie du rapport
#' @return Génère un rapport au format HTML
#' @import quarto
#' @export
generer_rapport <- function(commune, departement, output = "rapport.html") {
  # Cette fonction prend un code de commune, code de département, et un chemin pour le fichier de sortie en entrée et génère en retour un rapport quarto correspondant à la commune/ au département
  fichier_qmd <- system.file("rapport.qmd", package = "firstlib")

  if (file.exists(output)) {
    file.rename(output, paste0(output, "_old_", Sys.Date(), ".html"))
  }

  quarto::quarto_render(
    input = fichier_qmd,
    output_file = output,
    execute_params = list(
      code_commune = commune,
      code_departement = departement
    ))

  message("Le rapport a bien été généré : ", output)
}
