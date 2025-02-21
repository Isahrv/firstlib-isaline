# Test 1

test_that("creer_commune crée un objet de classe 'commune'", {
  df_commune <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
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
  commune <- creer_commune(df_commune)
  expect_s3_class(commune, "commune")
})

# Test 2

test_that("la classe d'origine est préservée", {
  df_commune <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
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
  class(df_commune) <- "data.frame"
  commune <- creer_commune(df_commune)
  expect_equal(class(commune), c("commune", "data.frame"))
})
