# Test 1

test_that("summary.departement retourne les informations correctes", {
  df <- data.frame(
    Code.du.département = c("44", "44"),
    Libellé.du.département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
    Code.de.la.commune = c("44109", "44109"),
    Libellé.de.la.commune = c("Nantes", "Rezé"),
    Nom.de.l.élu = c("Haouache", "Loas"),
    Prénom.de.l.élu = c("Zahia", "Lucie"),
    Code.sexe = c("F", "F"),
    Date.de.naissance = c("10/04/1980", "25/12/1975"),
    Code.de.la.catégorie.socio.professionnelle = c(1, 1),
    Libellé.de.la.catégorie.socio.professionnelle = c("Cadres", "Cadres"),
    Date.de.début.du.mandat = c("03/01/2020", "01/06/2022"),
    Libellé.de.la.fonction = c("Maire", "Maire"),
    Date.de.début.de.la.fonction = c("03/01/2020", "01/06/2022"),
    Code.nationalité = c("FR", "FR")
  )

  expect_output(summary.departement(df), "Nom du département : Loire-Atlantique")
  expect_output(summary.departement(df), "Nombre de commune : 2")
  })

# Test 2

test_that("summary.departement gère un dataframe vide", {
  df_vide <- data.frame(
    Code.du.département = character(),
    Libellé.du.département = character(),
    Code.de.la.collectivité.à.statut.particulier = character(),
    Libellé.de.la.collectivité.à.statut.particulier = character(),
    Code.de.la.commune = character(),
    Libellé.de.la.commune = character(),
    Nom.de.l.élu = character(),
    Prénom.de.l.élu = character(),
    Code.sexe = character(),
    Date.de.naissance = character(),
    Code.de.la.catégorie.socio.professionnelle = character(),
    Libellé.de.la.catégorie.socio.professionnelle = character(),
    Date.de.début.du.mandat = character(),
    Libellé.de.la.fonction = character(),
    Date.de.début.de.la.fonction = character(),
    Code.nationalité = character()
  )

  expect_error(summary.departement(df_vide), "Erreur in df$Nombre.d'élus : objet de type 'closure' non subsettable
")
})
