# Test 1

test_that("compter_nb_elus ignore les doublons correctement", {
  df <- data.frame(
    Code_du_département = c("44", "44"),
    Libellé_du_département = c("Loire-Atlantique", "Loire-Atlantique"),
    Code_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Libellé_de_la_collectivité_à_statut_particulier = c(NA, NA),
    Code_de_la_commune = c("44109", "44109"),
    Libellé_de_la_commune = c("Nantes", "Nantes"),
    Nom_de_l_élu = c("Dupont", "Dupont"),
    Prénom_de_l_élu = c("Jean", "Jean"),
    Code_sexe = c("M", "M"),
    Date_de_naissance = c("01/01/1970", "01/01/1970"),
    Code_de_la_catégorie_socio_professionnelle = c(1, 1),
    Libellé_de_la_catégorie_socio_professionnelle = c("Cadres", "Cadres"),
    Date_de_début_du_mandat = c("01/01/2020", "01/01/2020"),
    Libellé_de_la_fonction = c("Maire", "Maire"),
    Date_de_début_de_la_fonction = c("01/01/2020", "01/01/2020"),
    Code_nationalité = c("FR", "FR")
  )

  expect_equal(compter_nb_elus(df), 1)
})

# Test 2

test_that("compter_nb_elus retourne une erreur avec un schéma incorrect", {
  df_incorrect <- data.frame(
    Nom_de_l_élu = c("Haouache", "Loas"),
    Prénom_de_l_élu = c("Jean", "Victoria")
  )

  expect_error(compter_nb_elus(df_incorrect), "identical")
})
