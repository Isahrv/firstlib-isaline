# Test 1

test_that("calcul_distribution_age retourne bien des quantiles", {
  df <- data.frame(
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
  result <- calcul_distribution_age(df)
  expect_length(result, 5)
  expect_named(result, c("0%", "25%", "50%", "75%", "100%"))
})

# Test 2

test_that("calcul_distribution_age gère des élus ayant le même âge", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
    Nom_de_l_élu = c("Haouache", "Loas"),
    Prénom_de_l_élu = c("Zahia", "Lucie"),
    Code_sexe = c("F", "F"),
    Date_de_naissance = c("10/04/1980", "10/04/1980"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 1),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Cadres"),
    Date_de_début_du_mandat = c("03/01/2020", "01/06/2022"),
    Libellé_de_la_fonction = c("Maire", "Maire"),
    Date_de_début_de_la_fonction = c("03/01/2020", "01/06/2022"),
    Code_nationalité = c("FR", "FR")
  )
  result <- calcul_distribution_age(df)
  expect_length(result, 5)
  expect_named(result, c("0%", "25%", "50%", "75%", "100%"))
})
