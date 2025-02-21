# Test 1

test_that("summary.departement retourne les informations correctes", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Rezé"),
    Nom_de_l_élu = c("Haouache", "Loas"),
    Prénom_de_l_élu = c("Zahia", "Lucie"),
    Code_sexe = c("F", "F"),
    Date_de_naissance = c("10/04/1980", "25/12/1975"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 1),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Cadres"),
    Date_de_début_du_mandat = c("03/01/2020", "01/06/2022"),
    Libellé_de_la_fonction = c("Maire", "Maire"),
    Date_de_début_de_la_fonction = c("03/01/2020", "01/06/2022"),
    Code_nationalité = c("FR", "FR")
  )

  expect_output(summary.departement(df), "Nom du département : Loire-Atlantique")
  expect_output(summary.departement(df), "Nombre de commune : 2")
  })

# Test 2

test_that("summary.departement gère des NA dans le libellé du département", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c(NA, "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Rezé"),
    Nom_de_l_élu = c("Haouache", "Loas"),
    Prénom_de_l_élu = c("Zahia", "Lucie"),
    Code_sexe = c("F", "F"),
    Date_de_naissance = c("10/04/1980", "25/12/1975"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 1),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Cadres"),
    Date_de_début_du_mandat = c("03/01/2020", "01/06/2022"),
    Libellé_de_la_fonction = c("Maire", "Maire"),
    Date_de_début_de_la_fonction = c("03/01/2020", "01/06/2022"),
    Code_nationalité = c("FR", "FR")
  )

  expect_output(summary.departement(df), "Nom du département : Loire-Atlantique")
  expect_output(summary.departement(df), "Nombre de commune : 2")
})
